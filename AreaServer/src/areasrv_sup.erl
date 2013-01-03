-module(areasrv_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
    ]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 30,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    

    LibSup = {'lib_sup', {lib_sup, start_link, []},
        Restart, infinity, supervisor, dynamic},

    AreaSrvSup = {'areasrv', {areasrv, start_link, [event_handler]},
        Restart, Shutdown, worker, dynamic},

    {ok, {SupFlags, [
        AreaSrvSup,
        LibSup
        ]}}.


