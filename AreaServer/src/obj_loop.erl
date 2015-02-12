%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This module contains the object loop that every object executes.
%% @end
%%----------------------------------------------------------------------
-module(obj_loop).
-author("christian@flodihn.se").

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% Exports for fitting into a supervisor tree
-export([
    start_link/2
    ]).

% Internal exports
-export([
    loop_init/2,
    loop/1,
    handle_msg/2
    ]).

%%----------------------------------------------------------------------
%% @spec start_link(Arg, Type) -> {ok, Pid}
%% where
%%      Arg = any()
%%      Type = any()   
%% @doc
%% Starts a new object with a new state. Arg should either be new_state or 
%% {existing_state, State}, Type should be a tuple {type, Mod} where Mod
%% is the implementation module of the object, for example obj, player or 
%% animal.
%% @end
%%----------------------------------------------------------------------
start_link({type, Type}, {new_state, NewState}) ->
    Pid = spawn_link(?MODULE, loop_init, [Type, NewState]),
    {ok, Pid};

start_link({inst_state, State}, {new_state, NewState}) ->
    Pid = spawn_link(?MODULE, loop_init, [State, NewState]),
    {ok, Pid}.

%%----------------------------------------------------------------------
%% @spec loop_init(Type) -> ok
%% where
%%      Type = atom()    
%% @doc
%% Starts the object loop with a new state.
%% @end
%%----------------------------------------------------------------------
loop_init(Type, NewState) when is_atom(Type), is_list(NewState) ->
    {ok, State} = Type:new(),
    [obj:set_property(Key, Val) || {Key, Val} <- NewState],
    {ok, InitState} = Type:init(State#obj{type=Type}),
    loop(InitState);

%%----------------------------------------------------------------------
%% @spec loop_init(Type, State) -> ok
%% where
%%      Type = atom(),
%%      State = obj()
%% @doc
%% Starts the object loop with an existing state.
%% @end
%%----------------------------------------------------------------------
loop_init(#obj{type=Type} = State, NewState) when is_list(NewState)->
    [obj:set_property(Key, Val) || {Key, Val} <- State#obj.properties],
    [obj:set_property(Key, Val) || {Key, Val} <- NewState],
    % Since we put all state in the process dict, it would be a waste
    % of memory to have it in the state as well.
    {ok, InitState} = Type:init(State#obj{properties=[]}),
    loop(InitState).

%%----------------------------------------------------------------------
%% @spec loop(State) -> ok
%% where
%%      State = obj()
%% @doc
%% Object loop for all types of objects.
%% @end
%%----------------------------------------------------------------------
loop(#obj{tick_interval=TickInterval} = State) ->
    do_tick(State),
    receive
        Msg ->
            {ok, NewState} = ?MODULE:handle_msg(Msg, State),
            ?MODULE:loop(NewState)
    after TickInterval ->
            ?MODULE:loop(State)
    end.

handle_msg(Message, #obj{type=Type} = State) ->
    case Message of
        {command, From, Command, Args} ->
            Type:command_chain(From, Command, Args, State),
            {ok, State};
        {sync_command, From, CommandId, Command, Args} ->
            Result = Type:command_chain(From, Command, Args, State),
            From ! {CommandId, {result, Result}},
            {ok, State};
        {event, From, Event, Args} ->
            Type:event_chain(From, Event, Args, State),
            {ok, State};
        {transform, NewType} ->
            NewState = State#obj{type=NewType},
            {ok, NewInitState} = NewType:init(NewState),
            {ok, NewInitState};
        {client_request, Request, CharInfo} ->
            Type:client_request(Request, State, CharInfo),
            {ok, State};
        {obj_reply, From, Reply} ->
            Type:reply_chain(From, Reply, State),
            {ok, State};
        {set_tick, {interval, TickInterval}} ->
            {ok, State#obj{tick_interval=TickInterval}};
        % Always receiving a wildcard match in the inbox prevents 
        % memory leaks.
        Other ->
            error_logger:info_report([{?MODULE, unknown_message, Other}]),
            {ok, State}
    end.

do_tick(#obj{type=Type, tick_interval=TickInterval} = State) ->
    case obj:get_property(last_tick) of 
        undefined ->
            obj:set_property(last_tick, now());
        LastTick when is_tuple(LastTick) ->
            Now = now(),
            case timer:now_diff(Now, LastTick) > TickInterval of
                true ->
                    obj:set_property(last_tick, Now),
                    Type:tick_chain(LastTick, State);
                false ->
                    pass
            end
    end.
