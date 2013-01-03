-module(libgeo_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibGeo = {'LibGeo', {libgeo, start_link, [geo_funs]},
        Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibGeo]}}.

