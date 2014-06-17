-module(libdist_balancer).

-include_lib("stdlib/include/qlc.hrl").
-include("obj_registry.hrl").

-define(LOAD_DIFF, 1).

% API
-export([
    init/0,
	create_table/0,
	join/1,
    get_load/0,
    get_load/1,
	register_load/0,
	register_load/1
    ]).

% handlers
-export([
    ]).

% internal exports
-export([
    balance_init/0,
    balance/0
    ]).

-record(srvload, {node, load}).

init() ->
    application:start(sasl),
    application:start(os_mon),
    cpu_sup:util(), % Avoid first time garbage value.
    case whereis(?MODULE) of
	    undefined ->
	        spawn_link(?MODULE, balance_init, []);
    	Pid ->
	        case is_process_alive(Pid) of
		        true ->
		            {error, balancer_already_running};
		        false ->
		            unregister(?MODULE),
		            spawn_link(?MODULE, balance_init, [])
	        end
    end.

create_table() ->
	mnesia:start(),
    mnesia:create_table(srvload, [{ram_copies, [node()]},
        {attributes, record_info(fields, srvload)},
        {type, ordered_set}]).

join(Node) ->
	rpc:call(Node, mnesia, add_table_copy,
		[srvload, self(), ram_copies]),
	mnesia:change_config(extra_db_nodes, [Node]).

get_load() ->
    get_load(node()).

get_load(highest) ->
    LoadList = get_areasrv_load(),
    highest_load(LoadList);

get_load(lowest) ->
    LoadList = get_areasrv_load(),
    lowest_load(LoadList);

get_load(Node) ->
    F =  fun() ->
        mnesia:read(srvload, Node)
    end,
    case mnesia:transaction(F) of
        {atomic, [AreaSrvLoad]} ->
            AreaSrvLoad#srvload.load;
        _Error ->
            {error, no_table} 
    end.

get_areasrv_load() ->
    F =  fun() ->
        qlc:e(qlc:q([X || X <- mnesia:table(srvload)]))
    end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic, LoadList} ->
            LoadList;
        _Error ->
            []
    end.

highest_load([]) ->
    {error, empty_list};

highest_load([{_, Node, Load} | Tail]) ->
    highest_load(Tail, {Node, Load}).

highest_load([{_, Node, Load} | Tail], {_AreaSrv, Highest}) when Load >= Highest ->
    highest_load(Tail, {Node, Load});
 
highest_load([{_, _, Load} | Tail], {_AreaSrv, Highest} = H) when Load < Highest ->
    highest_load(Tail, H);
      
highest_load([], Highest) ->
    Highest.

lowest_load([]) ->
    {error, empty_list};

lowest_load([{_, Node, Load} | Tail]) ->
    lowest_load(Tail, {Node, Load}).

lowest_load([{_, Node, Load} | Tail], {_AreaSrv, Lowest}) when Lowest >= Load ->
    lowest_load(Tail, {Node, Load});
 
lowest_load([{_, _, Load} | Tail], {_AreaSrv, Lowest} = H) when Load > Lowest->
    lowest_load(Tail, H);
      
lowest_load([], Lowest) ->
    Lowest.

calc_load() ->
    round(cpu_sup:avg1()/256).

register_load() ->
    Load = calc_load(),
    register_load(Load).

register_load(Load) ->
    F = fun() ->
        mnesia:write(#srvload{node=node(), load=Load})
    end,
    mnesia:transaction(F).

do_migrate() ->
    % Try migrate half the objects each time.
    case mnesia:table_info(obj_registry, size) of
        0 ->
            pass;
        MigrateNr ->
            error_logger:info_report([{migration_started, {num_objs, 
                round(MigrateNr/2)}}]),
            do_migrate(round(MigrateNr/2), 0)
    end.

do_migrate(Nr, NrMigrated) ->
    {DestNode, _Load} = get_load(lowest),
    case libstd:next_obj() of
        '$end_of_table' ->
            migrate_report(NrMigrated);
        {ok, Obj} ->
            case migrate_obj(DestNode, Obj#obj_registry.pid) of
                {ok, migrated} ->
                    do_migrate(Nr - 1, NrMigrated + 1, 
                        Obj#obj_registry.ref);
                {error, _Reason} ->
                    do_migrate(Nr - 1, NrMigrated, Obj#obj_registry.ref)
            end;
        Other ->
            error_logger:info_report([{help, Other}])
    end.

do_migrate(0, MigrateNr, _Ref) ->
    migrate_report(MigrateNr);

do_migrate(Nr, NrMigrated, PrevRef) ->
    % Make a check so we dont migrate to ourselves.
    {DestNode, _Load} = get_load(lowest),
    case libstd:next_obj(PrevRef) of
        '$end_of_table' ->
            migrate_report(NrMigrated);
        {ok, Obj} ->
            case migrate_obj(DestNode, Obj#obj_registry.pid) of
                {ok, migrated} ->
                    do_migrate(Nr - 1, NrMigrated + 1, 
                        Obj#obj_registry.ref);
                {error, Reason} ->
                    error_logger:info_report({[migration_failed, Reason]}),
                    do_migrate(Nr, NrMigrated, Obj#obj_registry.ref)
            end
    end.

migrate_obj(DestNode, Pid) ->
    case node(Pid) =:= node() of
        true ->
            % The pid is local, migrate it.
            Pid ! {node_migrate, DestNode},
            %do_migrate(Nr - 1, NrMigrated + 1, Ref),
            {ok, migrated};
        false ->
            % The pid is on another node, jump to next
            %do_migrate(Nr, NrMigrated, Ref),
            {error, not_local_pid}
    end.

migrate_report(0) ->
    done;

migrate_report(NrMigrated) ->
    error_logger:info_report([{migration_finished, NrMigrated, from, node()}]).

balance_init() ->
    case os:type() of
	{win32, _} ->
        % load balacing is disabled for win32
	    exit(normal);
	_ ->
	    ok
    end,
    register(?MODULE, self()),
    ?MODULE:balance().

balance() ->
    receive 
        stop ->
            unregister(?MODULE),
            exit(normal)
    after 2000 ->
        case register_load() of
            {aborted, _Reason} ->
                %error_logger:info_report([{warning, no_table, Reason}]),
                ?MODULE:balance();
            {atomic, ok} ->
                {Node, LowestLoad} = get_load(lowest),
                check_balance(Node, LowestLoad),
                ?MODULE:balance()
        end
    end.

check_balance(Node, _LowestLoad) when Node == node() ->
    no_migration_to_self;

check_balance(_Node, LowestLoad) ->
    case calc_load() of
        Load when abs(Load - LowestLoad) >= ?LOAD_DIFF ->
            %error_logger:info_report([{start_migrate, to, Node, 
            %    diff, abs(Load - HighestLoad)}]),
            do_migrate();
        _Load ->
            do_nothing 
    end.

