-module(robot).

-include("obj.hrl").
-include("vec.hrl").

%-define(NEXT_TICK, random:uniform(5000) + 5000.
-define(NEXT_TICK, 1000.

-import(timer).
-import(io).

-import(obj, [call_self/2, call_self/3]).

-export([
    create_state/1,
    init/1,
    update_parents/1,
    post_init/1,
    heart_beat/2,
    change_dir/2,
    obj_dir/4,
    report/2,
    start_walking/2
    ]).

create_state(Type) ->
    {ok, State} = movable:create_state(Type),
    State2 = update_parents(State),
    {ok, State2}.

init(State) ->
    movable:init(State).

update_parents(State) ->
    State#obj{parents=[movable, obj]}.    

heart_beat(From, State) ->
    movable:heart_beat(From, State).

post_init(State) ->
    {ok, _Reply, NewState} = obj:call_self(set_property, 
        [mesh, "robot.mesh"], State),
    {ok, _Reply, NewState2} = obj:call_self(do_anim, ["Walk"], NewState),
    {ok, _Reply, NewState3} = obj:call_self(set_heart_beat, [1000], 
        NewState2),
    {ok, _Reply, NewState4} = obj:call_self(set_speed, [10], NewState3),
    movable:post_init(NewState4).

start_walking(_From, State) ->
    %error_logger:info_report(["Robot starting to walk..."]),
    X = util:rand_float(),
    Z = util:rand_float(),
    %X = 1,
    %Z = 1,
    Dir = #vec{x=X, z=Z},
    {ok, _Reply, NewState} = obj:call_self(set_dir, [Dir], State),
    {noreply, NewState}.

change_dir(_From, State) ->
    {ok, Vec, _} = call_self(get_dir, State),
    X = util:rand_float(),
    Z = util:rand_float(),
    NewVec = #vec{x=X, z=Z},
    %NewVec = #vec{x=-Vec#vec.x},
    {ok, _Reply, NewState} = obj:call_self(set_dir, [NewVec], State),
    NewState2 = report(self(), NewState),
    {noreply, NewState2}.

obj_dir(_From, _Id, _Vec, State) ->
    %case is_logging(State) of
    %    true ->
    %        NewState = increment_nr_msg(State),
    %        {noreply, NewState};
    %    false ->
    %        {noreply, State}
    %end.
    {noreply, State}.

report(_From, State) ->
    case call_self(get_property, [last_change_dir], State) of
        {ok, undefined, _} ->
            {ok, _Reply, NewState} = call_self(set_property, 
                [last_change_dir, now()], State),
            NewState;
        {ok, LastTime, _} ->
            TimeDiff = timer:now_diff(now(), LastTime) / 1000000,
            io:format("Last change_dir: ~p.~n", [TimeDiff]),
            {ok, _Reply, NewState} = call_self(set_property, 
                [last_change_dir, now()], State),
            NewState
    end.


