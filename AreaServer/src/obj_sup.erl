%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for all objects running in the node.
%% @end
%%----------------------------------------------------------------------

-module(obj_sup).
-behaviour(supervisor).

-include("obj.hrl").

% API.
-export([
    new/1,
    new/2,
    inst/1,
    inst/2
    ]).

% Supervisor callbacks.
-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = transient,
    Shutdown = 6000,

    ObjSup = {'Object', {obj_loop, start_link, []},
        Restart, Shutdown, worker, [obj_loop]},

    {ok, {SupFlags, [ObjSup]}}.


%%----------------------------------------------------------------------
%% @doc
%% @spec inst(State) ->{ok, Pid}
%% where
%%      State = obj()
%% @end
%% Instansiate an object with existing state. The State is an #obj 
%% record.
%% This function is used to existing objects from disk into the game.
%%----------------------------------------------------------------------
inst(#obj{} = State) ->
    inst(State, []).

%%----------------------------------------------------------------------
%% @doc
%% @spec inst(State, NewState) ->{ok, Pid}
%% where
%%      State = obj()
%%      NewState = list()
%% @end
%% Instansiate an object with existing state plus an additional
%% new state.
%% NewState is a list of {key, val} tuples and will override any
%% colliding keys in the State.
%%----------------------------------------------------------------------
inst(#obj{} = State, NewState) when is_list(NewState) ->
    supervisor:start_child(?MODULE, [
        {inst_state, State}, 
        {new_state, NewState}]).


%%----------------------------------------------------------------------
%% @doc
%% @spec new(Type, State) ->{ok, Pid}
%% where
%%      Type = atom(),
%%      State = obj()
%% @end
%% Create a new object without default state.
%%----------------------------------------------------------------------
new(Type) when is_atom(Type) ->
    new(Type, []).

%%----------------------------------------------------------------------
%% @doc
%% @spec new(Type, State) ->{ok, Pid}
%% where
%%      Type = atom(),
%%      State = list()
%% @end
%% Create a new object with a given state, the State is a list of
%% {Key, Val} tuples.
%%----------------------------------------------------------------------
new(Type, NewState) when is_atom(Type), is_list(NewState) ->
    supervisor:start_child(?MODULE, [
        {type, Type},
        {new_state, NewState}]).

