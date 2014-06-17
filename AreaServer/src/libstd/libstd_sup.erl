%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the standard library 'libstd'.
%% @end
%%----------------------------------------------------------------------

-module(libstd_sup).
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

    LibStd = {'libstd', {libstd_srv, start_link, [libstd_std_impl]},
        Restart, Shutdown, worker, dynamic},

    ObjSup = {'ObjectSupervisor', {obj_sup, start_link, []},
        Restart, Shutdown, supervisor, dynamic},
    
    {ok, {SupFlags, [LibStd, ObjSup]}}.

