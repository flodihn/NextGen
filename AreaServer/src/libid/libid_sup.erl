%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the id library 'libid'.
%% @end
%%---------------------------------------------------------------------
-module(libid_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, libid_sup}, libid_sup, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibPlayer = {'libid', {libid_srv, start_link, 
        [libid_std_impl]}, Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibPlayer]}}.

