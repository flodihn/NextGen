%%--------------------------------------------------------------------- 
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% This is the generic object, extended by all objects. It contain
%% functions for get/set properties and area migration.
%% @end
%%--------------------------------------------------------------------- 

-module(obj).
-author("christian@flodihn.se").

% Behaviours and namspaces does not work.
%-behaviour(libobj.gen_obj).

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% @headerfile "vec.hrl"
-include("vec.hrl").

%% @headerfile "shallow_object_info.hrl"
-include("shallow_object_info.hrl").

%% @headerfile "deep_object_info.hrl"
-include("deep_object_info.hrl").

% Chains (chain of command design pattern).
-export([
    tick_chain/2,
    command_chain/4,
    event_chain/4,
    reply_chain/3
    ]).

% Interface.
-export([
    new/0,
    init/1,
    destroy/1,
    set_tick/2,
    set_property/2,
    get_property/1,
    del_property/1,
    get_properties/0,
    transform/2,
    trigger_sync_command_chain/2,
    trigger_sync_command_chain/3,
    trigger_command_chain/2,
    trigger_command_chain/3,
    trigger_event_chain/4,
    obj_reply/2,
    send_event/1,
    send_event/2
    ]).

new() ->
    {ok, Id} = libid_srv:generate_id(),
    {ok, #obj{id=Id, type=?MODULE}}.

init(State) ->
    libstd_srv:register_obj(State#obj.id, self()),
    Pos = get_property(<<"pos">>),
    case Pos of 
        undefined ->
            %error_logger:info_report({?MODULE, warning, property, 
            %    <<"pos">>, undefined}),
            pass;
        #vec{} ->
            %NewQuad = libtree_srv:assign(State#obj.id, self(), 
            %    Pos, undefined),
            %set_property(quad, NewQuad)
            pass
    end,
    Dir = get_property(<<"dir">>),
    case Dir of 
        undefined ->
            set_property(<<"dir">>, #vec{x=0.0, y=0.0, z=0.0});
        #vec{} ->
            pass
    end,
    Mesh = get_property(<<"mesh">>),
    case Mesh of
        undefined ->
            set_property(<<"mesh">>, <<"NewObj.mesh">>);
        _ ->
            pass
    end,
    Scale = get_property(<<"scale">>),
    case Scale of
        undefined ->
            set_property(<<"scale">>, 1.0);
        _ ->
            pass
    end,
    %send_event(shallow_object_info, #shallow_object_info{
    %    id=State#obj.id,
    %    mesh=get_property(<<"mesh">>),
    %    pos=get_property(<<"pos">>),
    %    dir=get_property(<<"dir">>),
    %    scale=get_property(<<"scale">>)}),
    
    {ok, State}.

destroy(#obj{id=Id}) ->
    send_event(obj_leave, {id, Id}),
    libstd_srv:unregister_obj(Id),
    exit(normal).

tick_chain(_LastTick, State) ->
    {ok, State}.

set_tick(Pid, Interval) ->
    Pid ! {set_tick, {interval, Interval}}.

set_property(Key, Val) ->
    put(Key, Val).

get_property(Key) ->
    get(Key).

del_property(Key) ->   
    erase(Key).

get_properties() ->
    get().

transform(Pid, Type) ->
    Pid ! {transform, Type}.

trigger_sync_command_chain(Pid, Command) when is_pid(Pid) ->
    trigger_sync_command_chain(Pid, Command, []);

trigger_sync_command_chain(Id, Command) ->
    case libstd_srv:get_pid(Id) of
        {ok, Pid} ->
            trigger_sync_command_chain(Pid, Command, []);
        {error, no_obj} ->
            {error, no_obj}
    end.

trigger_sync_command_chain(Pid, Command, Args) when is_pid(Pid) ->
    CommandId = {cmd_id, make_ref()},
    Pid ! {sync_command, self(), CommandId, Command, Args},
    receive 
        {CommandId, {result, noreply}} ->
            {error, noreply};
        {CommandId, {result, {reply, Result}}} ->
            {ok, Result}
    after 1000 ->
        timeout
    end;

trigger_sync_command_chain(Id, Command, Args) ->
    case libstd_srv:get_pid(Id) of
        {ok, Pid} ->
            trigger_sync_command_chain(Pid, Command, Args);
        {error, no_obj} ->
            {error, no_obj}
    end.

trigger_command_chain(Pid, Command) when is_pid(Pid) ->
    trigger_command_chain(Pid, Command, []);

trigger_command_chain(Id, Command) ->
    case libstd_srv:get_pid(Id) of
        {ok, Pid} ->
            trigger_command_chain(Pid, Command);
        {error, no_obj} ->
            error_logger:error_report({?MODULE, 
                trigger_command_chain, no_obj, {id, Id}})
    end.

trigger_command_chain(Pid, Command, Args) when is_pid(Pid) ->
    Pid ! {command, self(), Command, Args};

trigger_command_chain(Id, Command, Args) ->
    case libstd_srv:get_pid(Id) of
        {ok, Pid} ->
            trigger_command_chain(Pid, Command, Args);
        {error, no_obj} ->
            error_logger:error_report({?MODULE, 
                trigger_command_chain, no_obj, {id, Id}})
    end.
    
% Perhaps add functions for things like this instead?    
command_chain(_From, get_pos, [], _State) ->
    {reply, obj:get_property(<<"pos">>)};

command_chain(_From, set_pos, Pos, #obj{id=Id}) ->
    CurrentQuad = get_property(quad),
    set_property(<<"pos">>, Pos),
    %NewQuad = libtree_srv:assign(Id, self(), Pos, CurrentQuad),
    %obj:set_property(quad, NewQuad),
    noreply;

command_chain(_From, print_state, [], State) ->
    io:format("PRINT_STATE: ~p.~nPROPERTIES: ~p~n", [State, get()]),
    noreply;

%command_chain(_From, add_property, {<<"mesh">>, Mesh}, #obj{id=Id}) ->
%    error_logger:info_report({?MODULE, add_property, mesh}),
%    set_property(<<"mesh">>, Mesh),
%    send_event(set_mesh, {Id, Mesh}),
%    {reply, added};

command_chain(_From, add_property, {<<"pos">>, Pos}, #obj{id=Id}) ->
    send_event(set_pos, {Id, Pos}),
    set_property(<<"pos">>, Pos);

command_chain(_From, add_property, {<<"dir">>, Dir}, #obj{id=Id}) ->
    send_event(set_dir, {Id, Dir}),
    set_property(<<"dir">>, Dir);

command_chain(_From, add_property, {<<"mesh">>, Mesh}, #obj{id=Id}) ->
    send_event(set_mesh, {Id, Mesh}),
    set_property(<<"mesh">>, Mesh);

command_chain(_From, add_property, {<<"scale">>, Scale}, #obj{id=Id}) ->
    send_event(set_scale, {Id, Scale}),
    set_property(<<"scale">>, Scale);

command_chain(_From, add_property, {Key, Value}, _State) ->
    error_logger:info_report({?MODULE, add_property, Key, Value}),
    set_property(Key, Value),
    {reply, added};

command_chain(_From, get_property, Key, _State) ->
    Prop = get_property(Key),
    {reply, Prop};

command_chain(_From, del_property, Key, _State) ->
    del_property(Key),
    {reply, deleted};

command_chain(_From, get_properties, [], _State) ->
    Props = get_properties(),
    {reply, Props};

command_chain(From, shallow_object_info, Args,  #obj{id=Id}) ->
    Mesh = get_property(<<"mesh">>),
    Pos = get_property(<<"pos">>),
    Dir = get_property(<<"dir">>),
    Scale = get_property(<<"scale">>),
    obj_reply(From, #shallow_object_info{
        id=Id,
        mesh=Mesh,
        pos=Pos,
        dir=Dir,
        scale=Scale});
    
command_chain(From, deep_object_info, _Args,  #obj{id=Id}) ->
    Mesh = get_property(<<"mesh">>),
    Pos = get_property(<<"pos">>),
    Dir = get_property(<<"dir">>),
    Scale = get_property(<<"scale">>),
    Props = get(),
    obj_reply(From, #deep_object_info{
        id=Id,
        mesh=Mesh,
        pos=Pos,
        dir=Dir,
        scale=Scale,
        props=Props});
    
command_chain(_From, get_id, [], State) ->
    {reply, State#obj.id};

command_chain(_From, save, [], #obj{id=Id} = State) ->
    Props = get_properties(),
    libsave_srv:save(Id, State#obj{properties=Props});

command_chain(From, get_actions, [], #obj{id=Id} = State) ->
    obj_reply(From, {action_list, []});

command_chain(_From, Command, Args, _State) ->
    {reply, unknown_command}.

trigger_event_chain(ObjPid, FromPid, Event, Args) ->
    ObjPid ! {event, FromPid, Event, Args}.

event_chain(_From, obj_enter, _Id, _State) ->
    pass;

event_chain(From, shallow_object_info, _Args, #obj{id=Id}) ->
    Pos = get_property(<<"pos">>),
    Mesh = get_property(<<"mesh">>),
    Dir = get_property(<<"dir">>),
    Scale = get_property(<<"scale">>),
    obj_reply(From, 
        #shallow_object_info{
            id=Id, 
            pos=Pos, 
            mesh=Mesh, 
            dir=Dir,
            scale=Scale});

event_chain(_From, Event, Args, #obj{id=_MyId, type=MyType}) ->
    {error, unknown_event}.

reply_chain(_From, Reply, _State) ->
    error_logger:info_report({?MODULE, reply_receieved_chain, 
        unknown_reply, Reply}).

send_event(Event) ->
    send_event(Event, []).

send_event(Event, Args) ->
    CurrentQuad = obj:get_property(quad),
    case CurrentQuad of
        undefined ->
            %error_logger:error_report({?MODULE, send_event, 
            %    "CurrentQuad is undefined, aborting"});
            pass;
        _CurrentQuad ->
           % error_logger:error_report({?MODULE, send_event, 
           %     CurrentQuad}),
           % libtree_srv:event(self(), CurrentQuad, Event, Args)
           pass
    end.
    
obj_reply(Pid, Reply) when is_pid(Pid) ->
    Pid ! {obj_reply, self(), Reply};

obj_reply(NotPid, _Reply) ->
    error_logger:info_report({obj_reply, aborting, not_pid, NotPid}).


