-module(libsave_sup).
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

    LibSave = {'libsave', {libsave, start_link, [save_funs]},
        Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibSave]}}.

