-module(conn_sup).
-behaviour(supervisor).

-export([
    start_conn/1,
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

    ConnSup = {'connection', {connection, start_link, []},
        Restart, Shutdown, worker, [connection]},

    {ok, {SupFlags, [ConnSup]}}.

start_conn(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

