-module(robot_leader).

-include("obj.hrl").
-include("vec.hrl").

%-define(NEXT_TICK, random:uniform(5000) + 5000.
%-define(NEXT_TICK, 5000).

-import(timer).
-import(io).

-import(obj, [call_self/2, call_self/3]).

-export([
    init/1,
    post_init/1,
    change_dir/2
    ]).

init(State) ->
    movable:init(State#obj{parents=[robot, movable, obj]}).

post_init(State) ->
    robot:post_init(State).

change_dir(From, State) ->
    robot:change_dir(From, State).

%change_dir(From, State) ->
%    {noreply, NewState} = robot:change_dir(From, State),
%    {ok, NewDir, _State} = obj:call_self(get_dir, State),
%    {ok, Group, _State} = obj:call_self(get_group, State),
%    callout(?NEXT_TICK),
%    case Group of
%        undefined ->
%            %error_logger:error_report([{group, undefined}]),
%            pass;
%        Group when is_pid(Group) ->
%            %error_logger:error_report([{calling_group, Group}]),
%            obj:async_call(Group, command, [set_dir, [NewDir]]);
%        Error ->
%            error_logger:error_report([{invalid_group, Error}])
%    end,
%    {noreply, NewState}.

    
%callout(Time) ->
%    erlang:send_after(Time, self(), {execute, {from, self()},
%        {call, change_dir}, {args, []}}).

