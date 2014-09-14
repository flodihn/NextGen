%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the player library 'libplayer'.
%% @end
%%---------------------------------------------------------------------
-module(liblog_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, liblog_sup}, liblog_sup, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibLogger = {'liblog', {liblog_srv, start_link, 
        [observer_logger]}, Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibLogger]}}.

