%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the supervisor for the environment library 'libenv'.
%% @end
%%----------------------------------------------------------------------
-module(libenv.sup).
-behaviour(supervisor).

-import(supervisor).

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

    LibEnv = {'libenv', {libenv.srv, start_link, [libenv.simple_env]},
        Restart, Shutdown, worker, dynamic},
    
    {ok, {SupFlags, [LibEnv]}}.

