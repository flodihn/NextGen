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

-export([
    start/0,
    start/1,
    start/2,
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

start() ->
    start(obj).

start(Type) ->
    supervisor:start_child(?MODULE, [new_state, {type, Type}]).

start(Type, State) ->
    supervisor:start_child(?MODULE, [{existing_state, State}, 
        {type, Type}]).


