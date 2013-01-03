%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This object extends the player object with additional god powers.
%% @end
%%----------------------------------------------------------------------
-module(god).

 %% @headerfile "obj.hrl"
-include("obj.hrl").

-export([
    create_state/1,
    init/1,
    post_init/2,
    update_parents/1,
    is/3,
    god_tool_create/6,
    god_tool_destroy/3,
    god_tool_change_property/5,
    god_tool_transform/4,
    increase_speed/2,
    decrease_speed/2
    ]).

create_state(Type) ->
    {ok, State} = player:create_state(Type),
    NewState = update_parents(State),
    {ok, NewState}.

init(State) ->
    player:init(State).

post_init(From, State) ->
    player:post_init(From, State),
    {ok, Conn, _State} = obj:call_self(get_property, [conn], State),
    error_logger:info_report([{enable_god_tool}]),
    Conn ! enable_god_tool,
    {noreply, State}.

update_parents(State) ->
    State#obj{parents=[player, movable, obj]}.
    

%%----------------------------------------------------------------------
%% @spec is(From, Type, State) ->{ok, true | false, State}
%% where
%%      From = pid(),
%%      Type = atom(),
%%      State = obj()
%% @doc
%% @see obj:is/3
%% @end
%%----------------------------------------------------------------------

is(_From, god, State) ->
    {reply, true, State};

is(From, Other, State) ->
    apply(player, is, [From, Other, State]).

god_tool_create(_From, {type, TypeBin}, {name, NameBin}, 
    {mesh, MeshBin}, {pos, Pos}, State) ->
    % Note that his is a security issue, should use list_to_existing_atom.
    Type = list_to_atom(binary_to_list(TypeBin)),
    % Meshes should be saved ash binaries.
    Mesh = binary_to_list(MeshBin),
    {ok, Pid} = obj_sup:start(Type),
    {ok, Id} = obj:call(Pid, get_id),
    obj:call(Pid, set_pos, [Pos]),
    obj:call(Pid, quadtree_assign),
    obj:call(Pid, set_name, [NameBin]),
    obj:call(Pid, set_mesh, [Mesh]),
    error_logger:info_report([{god_tool, created_object, Id, NameBin, Type, Mesh,
        Pos}]),
    % Should this be moved to a post_init called manually?
    obj:call(Pid, post_init),
    obj:async_call(Pid, save),
    {noreply, State}.

god_tool_destroy(_From, Id, State) ->
    error_logger:info_report([{destroying_object, Id}]),
    {noreply, State}.

god_tool_change_property(_From, Id, Property, Value, State) ->
    error_logger:info_report([{change_property, Id, Property, Value}]),
    {noreply, State}.

god_tool_transform(_From, Id, NewType, State) ->
    error_logger:info_report([{tranform, Id, NewType}]),
    {noreply, State}.

% Overload the player increase/decrease functions, we dont want to
% set animations etc.
increase_speed(_From, #obj{id=Id} = State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    NewSpeed = OldSpeed + 1,
    {ok, _Reply, NewState} = obj:call_self(set_speed, [NewSpeed], State),
    {noreply, NewState}.

decrease_speed(_From, #obj{id=Id} = State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    NewSpeed = OldSpeed -1,
    {ok, _Reply, NewState} = obj:call_self(set_speed, [NewSpeed], State),
    {noreply, NewState}.

