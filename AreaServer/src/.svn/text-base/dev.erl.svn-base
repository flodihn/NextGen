-module(dev).

-include("obj.hrl").
-include("vec.hrl").

-export([
    create_robots/1,
    create_robot_group/1,
    create_robot_groups/2
    ]).

create_robots(Nr) ->
    create_robots(Nr, []).

create_robots(0, Acc) ->
    Acc;

create_robots(Nr, Acc) ->
    {ok, P} = obj_sup:start(robot),
    X = util:rand_int(10000),
    Z = util:rand_int(10000),

    %X = util:rand_int(50),
    %Z = util:rand_int(50),
    obj:async_call(P, set_pos, [#vec{x=X, z=Z}]),
    %obj:async_call(P, enable_logging),
    obj:async_call(P, quadtree_assign),
    obj:async_call(P, start_walking),
    create_robots(Nr - 1, [P | Acc]).

create_robot_groups(0, _GroupSize) ->
    done;

create_robot_groups(NrGroups, GroupSize) ->
    create_robot_group(GroupSize),
    create_robot_groups(NrGroups - 1, GroupSize).

create_robot_group(Nr) ->
    {ok, Group} = obj_sup:start(libobj.group),
    {ok, Leader} = obj_sup:start(robot_leader),
    %X = util:rand_int(10000),
    %Z = util:rand_int(10000),

    X = util:rand_int(50) + 200,
    Z = util:rand_int(50) + 200,
    LeaderPos = #vec{x=X, z=Z},
    obj:async_call(Leader, set_pos, [LeaderPos]),
    {ok, Id} = obj:call(Leader, get_id),
    obj:async_call(Group, add, [Id, Leader]),
    obj:async_call(Leader, quadtree_assign),
    obj:async_call(Leader, start_walking),
    %obj:async_call(Leader, log),
    %error_logger:info_report([{"Creating group at position", LeaderPos}]),
    create_robot_group(Nr - 1, Group, LeaderPos, [Leader]).

create_robot_group(0, Group, _Pos, Acc) ->
    {Group, Acc};

create_robot_group(Nr, Group, Pos, Acc) ->
    NewPos = util:vector_add(Pos, #vec{x=1, z=-1}),
    {ok, Pid} = obj_sup:start(robot),
    obj:call(Pid, set_pos, [NewPos]),
    obj:call(Pid, quadtree_assign),
    {ok, Id} = obj:call(Pid, get_id),
    obj:async_call(Group, add, [Id, Pid]),
    error_logger:info_report([{"Creating robot at pos", NewPos}]),
    create_robot_group(Nr - 1, Group, NewPos, [Pid | Acc]).

