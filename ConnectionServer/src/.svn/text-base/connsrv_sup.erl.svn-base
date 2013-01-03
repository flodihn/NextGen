-module(connsrv_sup).
-behaviour(supervisor).

-export([
    start/0,
    start_link/1,
    init/1
    ]).

start() ->
    spawn(fun() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg=[])
    end).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    ConnSrvSup = {'connsrv', {connsrv, start_link, [conn_dispatcher]},
        Restart, Shutdown, worker, dynamic},

    ConnSup = {'conn_sup', {conn_sup, start_link, []},
        Restart, infinity, supervisor, dynamic},

    {ok, {SupFlags, [ConnSrvSup, ConnSup]}}.

