%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% @end
%%----------------------------------------------------------------------
-module(libtree.flat_quads).

-import(error_logger).
-import(pg2).
-import(io).
-import(obj).

-include("vec.hrl").

% API
-export([
    init/0
    ]).

% handlers

-export([
    increase/1,
    decrease/1,
    info/1,
    assign_dispatch/3,
    assign/3,
    event_dispatch/4,
    event/4
    ]).

% Internal exports
-export([
    send_event/4
    ]).


%% @type state(). #state{depth = int()}
-record(state, {matrix_size=1, area_size=10000, area_name}).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the libtree library.
%% @end
%%----------------------------------------------------------------------
init() ->
    AreaName = libstd.srv:area_name(),
    State = #state{area_name=AreaName},
    create_quads(State),
    {ok, State}.

%%----------------------------------------------------------------------
%% @spec increase() -> {ok, Quads}
%% where
%%      State = state(),
%%      Quads= int()
%% @doc
%% Divides the current quad or quads in four. Returns the new total
%% number of quads.
%% @end
%%----------------------------------------------------------------------
increase(#state{matrix_size=MatrixSize, area_size=AreaSize} = State) ->
    NewMatrixSize = MatrixSize * 2,
    NewState = State#state{matrix_size=NewMatrixSize},
    create_quads(NewState),
    error_logger:info_report([
        {quads, round(NewMatrixSize * NewMatrixSize)}, 
        {quad_size, round(AreaSize/NewMatrixSize)}]),
    {ok, {ok, NewMatrixSize * NewMatrixSize},  NewState}.

%%----------------------------------------------------------------------
%% @spec decrease(State) -> {ok, Quads}
%% where
%%      State = state(),
%%      Quads= int()
%% @doc
%% Join all the current quads into one bigger. Returns the new total 
%% number of quads.
%% @end
%%----------------------------------------------------------------------
decrease(#state{matrix_size=MatrixSize, area_size=AreaSize} = State) ->
    NewMatrixSize = MatrixSize /2,
    NewState = State#state{matrix_size=NewMatrixSize},
    error_logger:info_report([{quads, MatrixSize * MatrixSize}, 
        {quad_size, AreaSize/MatrixSize}]),
    {ok, {ok, NewMatrixSize * NewMatrixSize}, NewState}.

%%----------------------------------------------------------------------
%% @spec info(State) -> ok
%% where
%%      State = state()
%% @doc
%% Returns information about the current state of the tree.
%% @end
%%---------------------------------------------------------------------
info(#state{matrix_size=MatrixSize, area_size=AreaSize,
    area_name=AreaName}) ->
    io:format("Matrix: ~p.~nQuads: ~p.~nQuad size: ~p.~nArea Size:~p.~nAreaName: ~p.~n", 
        [MatrixSize, MatrixSize*MatrixSize, AreaSize/MatrixSize,
        AreaSize, AreaName]).

%%----------------------------------------------------------------------
%% @private
%% @spec create_quads(State) -> ok
%% where
%%      State = state()
%% @doc
%% Returns the current depth level.
%% @end
%%----------------------------------------------------------------------
create_quads(#state{matrix_size=MatrixSize, area_name=AreaName}) ->
    create_quads(0, 0, MatrixSize, AreaName).

create_quads(Col, _Row, MatrixSize,_AreaName) when Col > MatrixSize ->
    done;

create_quads(Col, Row, MatrixSize, AreaName) when Row == MatrixSize ->
    %io:format("Creating quad: ~p_~p.~n", [Col, Row]),
    create_quad(Col, Row, AreaName),
    create_quads(Col + 1, 0, MatrixSize, AreaName);
    
create_quads(Col, Row, MatrixSize, AreaName) ->
    %io:format("Creating quad: ~p_~p.~n", [Col, Row]),
    create_quad(Col, Row, AreaName),
    create_quads(Col, Row + 1, MatrixSize, AreaName).

create_quad(Col, Row, AreaName) ->
    GroupName = generate_group_name(AreaName, Col, Row),
    pg2:create(GroupName),
    %case whereis(GroupName) of
    %    undefined ->
    %        %{ok, Pid} = flat_quad:start(GroupName),
    %        register(GroupName, Pid);
    %    _Pid ->
    %        pass
    %end,
    %io:format("Creating quad: ~p.~n", [GroupName]).
    ok.

generate_group_name(AreaName, Col, Row) ->
    list_to_atom(atom_to_list(AreaName) ++ "_" ++ integer_to_list(Col) ++
        "_" ++ integer_to_list(Row)).

assign_dispatch(Obj, Vec, State) ->
    spawn(?MODULE, assign, [Obj, Vec, State]).

% This avoids assigning to a negative quad.
assign(Obj, #vec{x=X, z=Z}, State) when X < 0 ->
    assign(Obj, #vec{x=0, z=Z}, State);

assign(Obj, #vec{x=X, z=Z}, State) when Z < 0 ->
    assign(Obj, #vec{x=X, z=0}, State);

assign(Obj, #vec{x=X, z=Z}, State) when X <0, Z < 0 ->
    assign(Obj, #vec{x=0, z=0}, State);

assign(Obj, #vec{x=X, z=Z}, #state{area_name=AreaName, 
    matrix_size=MatrixSize, area_size=AreaSize}) when is_pid(Obj) ->
    QuadSize = round(AreaSize/MatrixSize),
    Col = trunc(X/QuadSize),
    Row = trunc(Z/QuadSize),
    AssignedQuad = generate_group_name(AreaName, Col, Row),
    case obj:call(Obj, get_property, [current_quad]) of
        {ok, undefined} ->
            pg2:join(AssignedQuad, Obj),
            obj:async_call(Obj, set_property, [current_quad, 
                AssignedQuad]);
        {ok, AssignedQuad} ->
            pass;
        {ok, CurrentQuad} ->
            {ok, Id} = obj:call(Obj, get_id),
            send_event(pg2:get_members(CurrentQuad), Obj, obj_leave, [Id]),
            pg2:join(AssignedQuad, Obj),
            obj:async_call(Obj, set_property, [current_quad, 
                AssignedQuad]),
            pg2:leave(CurrentQuad, Obj),
            spawn(?MODULE, send_event, [pg2:get_members(AssignedQuad), Obj, obj_enter, [Id]])
    end,
    %error_logger:info_report([{quad_size, QuadSize, pos, X, Y, Z,
    %    AssignedQuad}]),
    ok.

event_dispatch(From, Fun, Args, State) ->
    spawn(?MODULE, event, [From, Fun, Args, State]).

event(From, Fun, Args, _State) when is_pid(From) ->
    case obj:call(From, get_property, [current_quad]) of
        {ok, undefined} ->
            pass;
        {ok, Quad} ->
            spawn(?MODULE, send_event, [pg2:get_members(Quad), From, Fun, Args])
    end.

send_event([], _From, _Fun, _Args) ->
    done;

send_event([To | Tail], From, Fun, Args) when To == From ->
    send_event(Tail, From, Fun, Args);

send_event([To | Tail], From, Fun, Args) ->
    obj:async_call(From, To, Fun, Args), 
    send_event(Tail, From, Fun, Args).    

