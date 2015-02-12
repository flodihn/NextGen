%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% @end
%%----------------------------------------------------------------------
-module(movable).

 %% @headerfile "obj.hrl"
-include("obj.hrl").
-include("vec.hrl").

-define(PARENT, obj).
-define(WALK_SPEED, 10.0).

-export([
    new/0,
    init/1,
    tick_chain/2,
    event_chain/4,
    command_chain/4,
    reply_chain/3
    ]).

% Private functions

update_pos(LastTick, Speed) ->
    case obj:get_property(<<"dir">>) of
        undefined ->
            pass;
        #vec{x=DirX, y=DirY, z=DirZ} ->
            #vec{x=PosX, y=PosY, z=PosZ} = 
                obj:get_property(<<"pos">>),
            TimePassed = timer:now_diff(now(), LastTick)/1000000,
            NewPos = #vec{
                x=PosX + (DirX * TimePassed * Speed),
                y=PosY + (DirY * TimePassed * Speed),
                z=PosZ + (DirZ * TimePassed * Speed)},
            error_logger:info_report({new_pos, NewPos}),
            obj:set_property(<<"pos">>, NewPos)
    end.

% Exported functions

new() ->
    {ok, Id} = libid_srv:generate_id(),
    {ok, #obj{id=Id, type=?MODULE}}.

init(State) ->
    obj:set_property(speed, 0),
    {ok, NewState} = ?PARENT:init(State),
    {ok, NewState}.

tick_chain(LastTick, State) ->
    case obj:get_property(<<"speed">>) of 
        undefined ->
            pass;
        0 ->
            pass;
        Speed ->
            % update our position depending on pos, dir and speed.
            update_pos(LastTick, Speed)
    end,
    ?PARENT:tick_chain(LastTick, State).

command_chain(_From, walk, [], _State) ->
    error_logger:info_report({walk}),
    obj:set_property(<<"speed">>, ?WALK_SPEED),
    {reply, ?WALK_SPEED};

command_chain(_From, stand, [], _State) ->
    error_logger:info_report({stand}),
    LastTick = obj:get_property(last_tick),
    Speed = obj:get_property(<<"speed">>),
    update_pos(LastTick, Speed),
    obj:set_property(<<"speed">>, 0),
    {reply, 0};

command_chain(_From, set_dir, {dir, Dir}, _State) ->
    error_logger:info_report([{set_dir, Dir}]),
    %obj:send_event(set_dir, {Dir, State#obj.id, TimeStamp}),
    obj:set_property(<<"dir">>, Dir),
    {reply, Dir};

command_chain(From, Function, Args, State) ->
    ?PARENT:command_chain(From, Function, Args, State).

event_chain(From, Event, Args, State) ->
    ?PARENT:event_chain(From, Event, Args, State).

reply_chain(From, Reply, State) ->
    ?PARENT:reply_chain(From, Reply, State).

