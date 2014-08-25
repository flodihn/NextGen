%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the player library 'libplayer'.
%% @end
%%---------------------------------------------------------------------
-module(libfaction_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, libfaction_sup}, libfaction_sup, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibFaction = {'libfaction', {libfaction_srv, start_link, 
        [twofaction_impl]}, Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibFaction]}}.

