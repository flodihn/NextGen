-module(monsrv_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1
    ]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {{one_for_one, 3, 10},
        [{monsrv,
            {monsrv, start_link, [mon]},
            permanent,
            10000,
            worker,
            [monsrv]}
        ]}}.


