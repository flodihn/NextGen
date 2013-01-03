-module(libtree.mnesia_quads).

-import(error_logger).
-import(mnesia).
-import(timer).
-import(lists).
-import(ets).
-import(rpc).
-import(io).

-import(util).
-import(obj).

-include("vec.hrl").

-export([
    init/0   
    ]).

-export([
    create_area/1,
    join_area/1,
    info/1,
    increase/1,
    decrease/1,
    assign/5,
    event/5,
    get_size/1,
    handle_exit/2
    ]).

% Internal exports
-export([
    send_message/4
    ]).


-record(state, {area_size=10000, tree_size=1, quad_size=10000, 
    quads=1}).

-record(obj, {id, pid}).

init() ->
    mnesia:start(),
    %State = #state{},
    %{ok, State}.
    % Have to fix so quad tree can join existing area if not created.
    create_area(#state{}).
    
create_area(State) ->
    % Create a quadtree of 256 quads (16 * 16) by default.
    {ok, NewState} = increase(State),
    {ok, NewState2} = increase(NewState),
    {ok, NewState3} = increase(NewState2),
    {ok, NewState4} = increase(NewState3),
    {ok, NewState4}.

join_area(Node) ->
    libstd.srv:monsrv_rpc(monsrv, get_area, 
        [libstd.srv:area_name()]),
    {ok, TreeSize} = rpc:call(Node, libtree.srv, get_size, []),
    join_tree(TreeSize, Node).

info(#state{area_size=AreaSize, quad_size=QuadSize, quads=Quads}) ->
    io:format("~nAreaSize: ~p.~nQuadSize: ~p.~nQuads: ~p.~n",
        [AreaSize, QuadSize, Quads]).

increase(#state{area_size=AreaSize, tree_size=TreeSize} = State) ->
    NewTreeSize = TreeSize * 2,
    QuadSize = AreaSize / NewTreeSize,
    build_tree(NewTreeSize, QuadSize),
    error_logger:info_report([{new_tree_size, NewTreeSize, QuadSize}]),
    {ok, State#state{tree_size=NewTreeSize, quad_size=QuadSize, 
        quads=NewTreeSize*NewTreeSize}}.

decrease(State) ->
    {ok, State}.

join_tree(TreeSize, Node) ->
    join_tree(1, 1, TreeSize, TreeSize, Node).

join_tree(MaxRow, MaxCol, MaxRow, MaxCol, Node) ->
    join_quad(MaxRow, MaxCol, Node);

join_tree(MaxRow, Col, MaxRow, MaxCol, Node) ->
    join_quad(MaxRow, Col, Node),
    join_tree(1, Col + 1, MaxRow, MaxCol, Node);

join_tree(Row, Col, MaxRow, MaxCol, Node) ->
    join_quad(Row, Col, Node),
    join_tree(Row + 1, Col, MaxRow, MaxCol, Node).

join_quad(Row, Col, Node) ->
    QuadName = get_quad_name(Row, Col),
    rpc:call(Node, mnesia, add_table_copy,
        [QuadName, node(), ram_copies]).

build_tree(TreeSize, QuadSize) ->
    build_tree(1, 1, TreeSize, TreeSize, QuadSize).

build_tree(MaxRow, MaxCol, MaxRow, MaxCol, _QuadSize) ->
    create_quad(MaxRow, MaxCol);

build_tree(MaxRow, Col, MaxRow, MaxCol, QuadSize) ->
    create_quad(MaxRow, Col),
    build_tree(1, Col + 1, MaxRow, MaxCol, QuadSize);

build_tree(Row, Col, MaxRow, MaxCol, QuadSize) ->
    create_quad(Row, Col),
    build_tree(Row + 1, Col, MaxRow, MaxCol, QuadSize).

create_quad(Row, Col) ->
    Name = get_quad_name(Row, Col),
    case lists:member(Name, mnesia:system_info(tables)) of
        true ->
            % Add connecting to existing table later
            pass;
        false ->
            mnesia:create_table(Name, [
                {ram_copies, [node()]}, 
                {record_name, obj},
                {attributes, record_info(fields, obj)}])
    end.



assign(Id, Obj, Pos, CurrentQuad, 
    #state{tree_size=TreeSize, quad_size=QuadSize} = TreeState) ->
    Row = util:trim_int(1, TreeSize, util:ceiling(Pos#vec.x/QuadSize)),
    Col = util:trim_int(1, TreeSize, util:ceiling(Pos#vec.z/QuadSize)),
    NewQuad = get_quad_name(Row, Col),
    case CurrentQuad of
        undefined ->
            % If there is no previous quad we write to the new.
            F = fun() ->
                mnesia:write(NewQuad, #obj{id=Id, pid=Obj}, write)
            end,
            mnesia:transaction(F),
            link(Obj),
            %error_logger:info_report([{R}]),
            %mnesia:dirty_write(NewQuad, #obj{id=Id, pid=Obj}),
            event(Obj, NewQuad, obj_enter, [Id], TreeState),
            NewQuad;
        NewQuad ->
            % If we have the same quad there is nothing to do.
            NewQuad;
        _NotSameQuad ->
            % If we are in a new quad, delete from old quad and write
            % to new.
            % Make a synchronous call to make sure we send the
            % leave notification in the old quad.
            event(Obj, CurrentQuad, obj_leave, [Id], TreeState),
            obj:async_call(Obj, quad_changed),
            %F = fun() ->
            %    mnesia:delete({CurrentQuad, Id}),
            %    mnesia:write(NewQuad, #obj{id=Id, pid=Obj})
            %end,
            %mnesia:transaction(F),
            mnesia:dirty_delete({CurrentQuad, Id}),
            mnesia:dirty_write(NewQuad, #obj{id=Id, pid=Obj}),
            unlink(Obj),
            event(Obj, NewQuad, obj_enter, [Id], TreeState),
            
            NewQuad
    end.

event(From, Quad, Fun, Args, _TreeState) ->
    spawn(?MODULE, send_message, [From, Quad, Fun, Args]).

get_quad_name(Row, Col) ->
    list_to_atom(integer_to_list(Row) ++ "_" ++ integer_to_list(Col)).

send_message(From, Quad, Fun, Args) ->
    Start = now(),
    FirstKey = mnesia:dirty_first(Quad),
    send_message(From, Quad, Fun, Args, FirstKey, Start).

send_message(_From, _Quad, _Fun, _Args, '$end_of_table', Start) ->
    End = now(),
    %error_logger:info_report([{send_message, time, 
    %    timer:now_diff(End, Start)/1000}]),
    done;

send_message(From, Quad, Fun, Args, Key, Start) ->
    [Obj] = mnesia:dirty_read(Quad, Key),
    obj:async_call(From, Obj#obj.pid, Fun, Args),
    NextKey = mnesia:dirty_next(Quad, Key),
    send_message(From, Quad, Fun, Args, NextKey, Start).

get_size(#state{tree_size=TreeSize}) ->
    {ok, TreeSize}.

handle_exit(Id, Quad) ->
    mnesia:dirty_delete(Quad, Id).
