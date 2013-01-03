-module(mon).

-export([
    init/1,
    init_db/0,
    cleanup/1,
    start/1,
    stop/0,
    register_node/1,
    get_area/1,
    %register_areasrv_load/3,
    %get_areasrv/1,
    %create_table/1,
    log/1,
    log/2,
    log_cpu/1,
    log_mem/1
    ]).

-define(LOGFILE, "system_resources.log").

-include("state.hrl").
-include("areas.hrl").
-include("area_count.hrl").

init(State) ->
    {ok, InitNodes} = application:get_env(monsrv, mem_check_nodes),
    rpc:multicall(InitNodes, net_kernel, connect_node, [node()]),
    application:start(sasl),
    application:start(os_mon),
    % Make a call to avoid first time garbage value.
    cpu_sup:util(),
    mnesia:start(),
    {ok, LogFile} = file:open(?LOGFILE, [append]),
    State#state{logfile=LogFile}.

init_db() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(areas, [{disc_copies, [node()]},
        {attributes, record_info(fields, areas)}]),
    mnesia:create_table(area_count, [{ram_copies, [node()]},
        {local_content, true},
        {attributes, record_info(fields, area_count)}]).

register_node(Node) ->
    AreaName = area_name(Node),
    F = fun() ->
        case mnesia:read(areas, AreaName) of
            [Area] ->
                mnesia:write(Area#areas{nodes=lists:umerge(Area#areas.nodes,
                    [Node])});
            [] ->
                mnesia:write(#areas{area=AreaName, nodes=[Node]})
        end
    end,
    mnesia:transaction(F).
      

get_area(Area) ->
    case mnesia:dirty_read(areas, Area) of
        [] ->
            {error, no_such_area};
        [AreaRec] ->
            Val = mnesia:dirty_update_counter(area_count, Area, 1),
            case Val >= length(AreaRec#areas.nodes) of
                true ->
                    F = fun() ->
                            mnesia:write(#area_count{area=Area, count=0})
                        end,
                    mnesia:transaction(F);
                false ->
                    ok
            end,
            lists:nth(Val, AreaRec#areas.nodes)
    end.

cleanup(#state{logfile=LogFile}) -> 
    file:close(LogFile).

%register_areasrv_load(AreaSrv, Node, Load) ->
%    F = fun() ->
%            mnesia:write(AreaSrv, #areasrv_load{node=Node, load=Load}, 
%                write)
%    end,
%    case mnesia:transaction(F) of
%        {atomic, Result} ->
%            Result;
%        %{aborted, {bad_type, _Record}} ->
%        Failure ->
%            error_logger:info_report([{failure, Failure}]),
%            create_table(AreaSrv),
%            mnesia:transaction(F)
%    end.

%create_table(AreaSrv) ->
%    mnesia:create_table(AreaSrv, [{ram_copies, [node()]},
%        {attributes, record_info(fields, areasrv_load)},
%        {type, ordered_set},
%        {record_name, areasrv_load}]),
%    error_logger:info_report([{created_load_table, AreaSrv}]).

%get_areasrv(AreaSrv) ->
%    Key = mnesia:dirty_first(AreaSrv),
%    get_areasrv(AreaSrv, Key, #areasrv_load{}).

%get_areasrv(_AreaSrv, {aborted, _Reason}, _Lowest) ->
%    undefined;

%get_areasrv(AreaSrv, '$end_of_table', #areasrv_load{node=Node}) ->
%    Node;

%get_areasrv(AreaSrv, Key, #areasrv_load{node=undefined, load=undefined}) ->
%    [Rec] = mnesia:dirty_read(AreaSrv, Key),
%    NextKey = mnesia:dirty_next(AreaSrv, Key),
%    get_areasrv(AreaSrv, NextKey, Rec);

%get_areasrv(AreaSrv, Key, #areasrv_load{load=Load} = Lowest) ->
%    [Rec] = mnesia:dirty_read(AreaSrv, Key),
%    NextKey = mnesia:dirty_next(AreaSrv, Key),
%    case Rec#areasrv_load.load < Lowest#areasrv_load.load of
%        true ->
%            get_areasrv(AreaSrv, NextKey, Rec);
%        false ->
%            get_areasrv(AreaSrv, NextKey, Lowest)
%    end.

log(Line, File) -> 
    Time = get_time(),
    file:write(File, io_lib:format("~s ~s~n", [Time, Line])).  

log(State) ->
    %{ok, ConnSrv} = application:get_env(monsrv, connsrv),
    %log_conn(ConnSrv, State),
    log_cpu(State),
    log_mem(State).

log_conn(ConnSrv, #state{logfile=LogFile} = State) ->
    Conns = rpc:call(ConnSrv, connsrv, get_connections, []),
    log("connections " ++ integer_to_list(Conns), LogFile).
    
log_cpu(#state{logfile=LogFile}) ->
    Log = sum_cpu(cpu_sup:util([per_cpu]), []),
    log("load " ++ Log, LogFile).

sum_cpu([{CPU, Busy, _NonBusy, _Misc}|Tail], Acc) ->
    Msg = io_lib:format("[cpu~p:~p%] ", [CPU, round(Busy)]),
    sum_cpu(Tail, lists:append(Acc, Msg));

sum_cpu([], Acc) ->
    lists:flatten(Acc).

log_mem(#state{logfile=LogFile}) ->
    %Log = sum_mem(nodes(), []),
    NodeList = nodes(),
    {MemList, _Empty} = rpc:multicall(NodeList, erlang, memory, [total]),
    Line = sum_mem(NodeList, MemList, []),
    log("memory " ++ Line, LogFile).

sum_mem([Node|NodeTail], [Mem|MemTail], Acc) ->
    NodeName = atom_to_list(area_name(Node)),
    MemMB = round(Mem/1048576),
    NodeMemLine = io_lib:format("[~s:~pMB] ", [NodeName, MemMB]),
    sum_mem(NodeTail, MemTail, string:concat(Acc, NodeMemLine));

sum_mem([], _, Acc) ->
    lists:flatten(Acc).

area_name(Node) ->
    [NodeName, _Host] = re:split(atom_to_list(Node), "@", [{return, list}]),
    list_to_atom(NodeName).

get_time() ->
    {{_Year, Month, Day}, {Hour, Min, Second}} = calendar:local_time(),
    MonthName = get_month(Month),
    {ok, Hostname} = inet:gethostname(),
    lists:flatten(io_lib:format("~s ~p ~p:~p:~p ~s ", 
        [MonthName, Day, Hour, Min, Second, Hostname])).

get_month(Month) ->
    case Month of
        1 ->
            "Jan";
        2 ->
            "Feb";
        3 ->
            "Mar";
        4 ->
            "Apr";
        5 ->
            "May";
        6 ->
            "Jun";
        7 ->
            "Jul";
        8 ->
            "Aug";
        9 ->
            "Sep";
        10 ->
            "Oct";
        11 ->
            "Nov";
        12 ->
            "Dec"
    end.


start(Interval) ->
    log_thread:init(Interval).

stop() ->
    case whereis(log_thread) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.
