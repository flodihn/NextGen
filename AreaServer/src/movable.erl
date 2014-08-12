%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Chrisitan Flodihn
%% @doc
%% This module implements a object with the possiblity to have a position
%% in the world and functions to move.
%% @end
%%----------------------------------------------------------------------
-module(movable).

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% @headerfile "vec.hrl"
-include("vec.hrl").

-import(obj, [
    call_self/2,
    call_self/3,
    async_call/3
    ]).

% The checkpoint value decides how often check for new quad is made.
-define(CHECKPOINT, 10).

-export([
    create_state/1,
    init/1,
    post_init/2,
    update_parents/1,
    is/3,
    heart_beat/2,
    query_entity/2,
    update_pos/2,
    set_checkpoint/3,
    get_checkpoint/2,
    set_speed/4,
    get_speed/2,
	jump/3,
	set_vector/3,
    enable_flying/2,
    disable_flying/2,
    get_max_speed/2,
    set_max_speed/3
    ]).

create_state(Type) ->
    {ok, State} = obj:create_state(Type),
    NewState = update_parents(State),
    {ok, NewState}.


%%----------------------------------------------------------------------
%% spec init(State) -> ok
%% where 
%%      State = obj()
%% @doc
%% Initiates the object for moving.
%% @end
%%----------------------------------------------------------------------
init(State) ->
    obj:init(State).

post_init(From, State) ->
    obj:post_init(From, State),
    {noreply, State}.


update_parents(State) ->
    State#obj{parents=[obj]}.

is(_From, movable, State) ->
    {reply, true, State};

is(From, Other, State) ->
    apply(obj, is, [From, Other, State]).

heart_beat(From, State) ->
    {ok, _Reply, NewState} = call_self(update_pos, State),
    obj:heart_beat(From, NewState).


set_speed(_From, Speed, TimeStamp, #obj{id=Id} = State) ->
    {ok, _Reply, NewState} = call_self(set_property, [speed, Speed], 
        State),
    obj:call_self(event, [obj_speed, [Id, Speed, TimeStamp]], NewState),
    case Speed of
        0 ->
            {ok, _Reply, NewState2} = call_self(update_pos, NewState),
            {noreply, NewState2};
        _Other ->
            {noreply, NewState}
    end.

get_speed(_From, State) ->
    case call_self(get_property, [speed], State) of
        {ok, undefined, _NewState} ->
            {reply, 0, State};
        {ok, Speed, _NewState} ->
            {reply, Speed, State}
    end.

%% @private
%update_pos(From, State) ->
%    {ok, Dir, _State} = call_self(get_dir, State),
%    {ok, Pos, _State} = call_self(get_pos, State),
%    {ok, Speed, _State} = call_self(get_speed, State),
%    if 
%        Dir == undefined; Pos == undefined; Speed == 0 ->
%            %call_self(log, [{update_pos, aborted}], State),
%            {noreply, State};
%        true ->
%            % Hmm should not the time also be a factor when calculating
%            % the new position?
%            Traveled = util:vector_mult(Dir, Speed),
%            NewPos = util:vector_add(Pos, Traveled),
%            %call_self(log, [{update_pos, NewPos}], State), 
%            {ok, _Reply, NewState} = call_self(set_pos, [NewPos], State),
%            update_checkpoint(From, NewState)
%    end.

% TODO: Update this function to sync with the new vector movement
update_pos(From, State) ->
    {ok, Dir, _State} = call_self(get_dir, State),
    {ok, Pos, _State} = call_self(get_pos, State),
    {ok, Speed, _State} = call_self(get_speed, State),
    if 
        Dir == undefined; Pos == undefined; Speed == 0 ->
            %call_self(log, [{update_pos, aborted}], State),
            {noreply, State};
        true ->
            % Hmm should not the time also be a factor when calculating
            % the new position?
            Traveled = util:vector_mult(Dir, Speed),
            NewPos = util:vector_add(Pos, Traveled),
            %call_self(log, [{update_pos, NewPos}], State), 
            {ok, _Reply, NewState} = call_self(set_pos, [NewPos], State),
            update_checkpoint(From, NewState)
    end.

update_checkpoint(From, State) ->
    {ok, Pos, _State} = call_self(get_pos, State),
    case call_self(get_checkpoint, State) of
        {ok, undefined, _State} ->
            {ok, _Reply, NewState} = call_self(set_checkpoint, [Pos], 
                State),
            {noreply, NewState};
        {ok, CheckPoint, _State} ->
            case util:vector_diff(Pos, CheckPoint) of
                Diff when Diff > 10 ->
                    {ok, _Reply, NewState} = call_self(set_checkpoint,
                        [Pos], State),
                    obj:quadtree_assign(From, NewState);
                _Diff  ->
                    {noreply, State}
            end
    end.

set_checkpoint(_From, Pos, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [checkpoint, Pos],
        State),
    {noreply, NewState}.

get_checkpoint(_From, State) ->
    {ok, CheckPoint, _State} = call_self(get_property, [checkpoint],
        State),
    {reply, CheckPoint, State}.

query_entity(From, #obj{id=Id} = State) ->
    case call_self(get_pos, State) of
        {ok, undefined, _} ->
            pass;
        {ok, Pos, _State} ->
            async_call(From, queried_entity, [{id, Id}, {key, pos},
                {value, Pos}])
    end,
    case call_self(get_dir, State) of
        {ok, undefined, _} ->
            pass;
        {ok, Dir, _} ->
            async_call(From, queried_entity, [{id, Id}, {key, dir},
                {value, Dir}])
    end,

    case cache:fetch(flying) of
        true ->
            async_call(From, queried_entity, [{id, Id}, {key, flying},
                {value, true}]);
        undefined ->
            pass
    end,
    % If speed is undefined, get_speed returns the integer 0, this should
    % really be changed to we don't need so send unnecessary data.
    {ok, Speed, _} = call_self(get_speed, State),
    async_call(From, queried_entity, 
        [{id, Id}, {key, speed}, {value, Speed}]),
    obj:query_entity(From, State).

enable_flying(_From, #obj{id=Id} = State) ->
    case call_self(get_property, [can_fly], State) of
        {ok, undefined, _State} ->
            % Just ingore
            {noreply, State};
        {ok, true, _State} ->
            call_self(event, [notify_flying, [Id, flying]], State),
            cache:cache(flying, true),
            {noreply, State}
    end.

disable_flying(_From, #obj{id=Id} = State) ->
    cache:cache(flying, undefined),
    call_self(event, [notify_flying, [Id, not_flying]], State),
    {noreply, State}.

get_max_speed(_From, State) ->
    case call_self(get_property, [max_speed], State) of 
        {ok, undefined, _State} ->
            {reply, 1, State};
        {ok, MaxSpeed, _State} ->
            {reply, MaxSpeed, State}
    end.

set_max_speed(_From, MaxSpeed, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [max_speed, MaxSpeed],
        State),
    {noreply, NewState}.

jump(_From, Force, #obj{id=Id} = State) ->
	call_self(event, [obj_jump, [Id, Force]], State),
    {noreply, State}.

set_vector(_From, Vector, #obj{id=Id} = State) ->
	call_self(event, [obj_vector, [Id, Vector]], State),
    {noreply, State}.
