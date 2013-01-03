-module(libtree.simple_quads).

-import(error_logger).
-import(ets).
-import(io).

-import(util).

-include("vec.hrl").

-export([
    init/0   
    ]).

-export([
    info/1,
    increase/1,
    decrease/1,
    assign/3
    ]).

-record(state, {area_size=10000, quad_size=0, quads=0, current_ets, 
    old_ets}).

init() ->
    TableId = ets:new(?MODULE, []),
    State = #state{current_ets=TableId},
    increase(State).

info(#state{area_size=AreaSize, quad_size=QuadSize, quads=Quads}) ->
    io:format("~nAreaSize: ~p.~nQuadSize: ~p.~nQuads: ~p.~n",
        [AreaSize, QuadSize, Quads]).

increase(#state{quads=Quads} = State) ->
    case Quads of
        0 ->
            create_new_tree(4, State);
        Nr ->
            create_new_tree(Nr * Nr, State)
    end.

create_new_tree(NrOfQuads, #state{area_size=AreaSize, 
    current_ets=OldTableId} = State) ->
    TableId = ets:new(?MODULE, []),
    QuadSize = AreaSize/NrOfQuads,
    build_tree(NrOfQuads, QuadSize, TableId),
    {ok, State#state{quads=NrOfQuads, quad_size=QuadSize,
        current_ets = TableId, old_ets=OldTableId}}.

decrease(State) ->
    {ok, State}.

create_quad(Row, Col, Size, TableId) ->
    {ok, Pid} = quad:start({Row, Col}, Size),
    ets:insert(TableId, {{Row, Col}, Pid}).

build_tree(Number, QuadSize, TableId) ->
    build_tree(1, 1, Number, Number, QuadSize, TableId).

build_tree(MaxRow, MaxCol, MaxRow, MaxCol, QuadSize, TableId) ->
    {ok, Pid} = quad:start({MaxRow, MaxCol}, QuadSize),
    ets:insert(TableId, {{MaxRow, MaxCol}, Pid});
    %error_logger:info_report([{creating_quad, MaxRow, MaxCol}]),

build_tree(MaxRow, Col, MaxRow, MaxCol, QuadSize, TableId) ->
    %error_logger:info_report([{creating_quad, MaxRow, Col}]),
    create_quad(MaxRow, Col, QuadSize, TableId),
    build_tree(1, Col + 1, MaxRow, MaxCol, QuadSize, TableId);

build_tree(Row, Col, MaxRow, MaxCol, QuadSize, TableId) ->
    create_quad(Row, Col, QuadSize, TableId),
    %error_logger:info_report([{creating_quad, Row, Col}]),
    build_tree(Row + 1, Col, MaxRow, MaxCol, QuadSize, TableId).

assign(Obj, Pos, #state{quad_size=QuadSize, 
    current_ets=TableId}) ->
    Row = util:ceiling(Pos#vec.x/QuadSize),
    Col = util:ceiling(Pos#vec.z/QuadSize),
    case ets:lookup(TableId, {Row, Col}) of
        [] ->
            error_logger:info_report([{?MODULE, no_quad, Obj, Pos, {Row, Col}}]),
            no_quad;
        [{_Key, Pid}] ->
            Pid ! {add_obj, Obj},
            error_logger:info_report([{?MODULE, assigning, Obj, to, Pid}]),
            Pid
    end.

