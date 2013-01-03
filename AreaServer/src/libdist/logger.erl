-module(libdist.logger).

-import(application).
-import(error_logger).
-import(cpu_sup).
-import(mnesia).
-import(timer).
-import(rpc).
-import(re).

% API
-export([
    init/0
    ]).

% handlers
-export([
    ]).

% internal exports
-export([
        start_logger/0,
        init_logger/0,
        loop/0,
        register_load/0
    ]).

init() ->
    case whereis(?MODULE) of 
        undefined ->
            ?MODULE:start_logger();
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    {error, already_started};
                false ->
                    ?MODULE:start_logger()
            end
    end.

start_logger() ->
    P = spawn_link(?MODULE, init_logger, []),
    {ok, P}.

init_logger() ->
    cpu_sup:start(),
    case whereis(?MODULE) of
        undefined ->
            register(?MODULE, self());
        Pid ->
            unregister(?MODULE),
            register(?MODULE, self())
    end,
    {ok, MonSrv} = application:get_env(areasrv, monsrv),
    put(monsrv, MonSrv),
    application:start(os_mon),
    application:start(sasl),
    cpu_sup:util(), % Avoid first time garbage value.
    ?MODULE:loop().

loop() ->
    timer:sleep(1000),
    ?MODULE:register_load(),
    ?MODULE:loop().

register_load() ->
    MonSrv = get(monsrv),
    %Load = cpu_sup:avg1(),
    % Oneliner to get short name as atom
    %AreaSrv = list_to_atom(binary_to_list(hd(re:split(
    %       atom_to_list(node()), "@")))),
    %R = rpc:call(MonSrv, mon, register_areasrv_load, [AreaSrv, node(), Load]),
    %error_logger:info_report([{logged_load_to, MonSrv, AreaSrv, R}]).
    S = mnesia:table_info(obj_registry, size), 
    %rpc:call(MonSrv, monsrv, log, ["robots: " ++ integer_to_list(S)]).
    rpc:call(monsrv@client, monsrv, log, ["robots: " ++ integer_to_list(S)]).


