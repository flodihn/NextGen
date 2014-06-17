%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the tree algoritm library 'libtree'.
%% @end
%%---------------------------------------------------------------------
-module(libtree_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, libtree_sup}, libtree_sup, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent, 
    Shutdown = 2000,

    LibTree = {'libtree', {libtree_srv, start_link, 
        [libtree_mnesia_quads]}, Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibTree]}}.

