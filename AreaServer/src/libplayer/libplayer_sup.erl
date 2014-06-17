%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the player library 'libplayer'.
%% @end
%%---------------------------------------------------------------------
-module(libplayer_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, libplayer_sup}, libplayer_sup, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibPlayer = {'libplayer', {libplayer_srv, start_link, 
        [libplayer_std_impl]}, Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibPlayer]}}.

