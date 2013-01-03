-module(lib_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start/1,
    start/2,
    stop/1,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    %Restart = transient,
    %Shutdown = 6000,
    
    {ok, {SupFlags, []}}.
    %ignore.

start(Lib) ->
    start(Lib, []).

start(Lib, Args) ->
    ChildSpec = {Lib, {Lib, start_link, Args}, 
        transient, infinity, supervisor, dynamic},
    supervisor:start_child(?MODULE, ChildSpec).

stop(Lib) ->
    supervisor:terminate_child(?MODULE, Lib),
    supervisor:delete_child(?MODULE, Lib).

