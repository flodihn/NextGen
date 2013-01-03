%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the god library 'libgod'.
%% @end
%%---------------------------------------------------------------------
-module(libgod.sup).
-behaviour(supervisor).

-import(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, libgod.sup}, libgod.sup, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibGod = {'libgod', {libgod.srv, start_link, 
        [libgod.std_impl]}, Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibGod]}}.

