-module(libtree_quad).

-export([
    start/2
    ]).

% Internal exports
-export([
    loop/1,
    send_all/3
    ]).

-record(state, {size, pos, objects=[]}).

start(Pos, Size) ->
    error_logger:info_report([{starting_quad, Pos, Size}]),
    Pid = spawn(?MODULE, loop, [#state{pos=Pos, size=Size}]),
    {ok, Pid}.

loop(#state{size=_Size, objects=Objects} = State) ->
    receive 
        {add_obj, Obj} ->
            NewObjects = lists:append(Objects, [Obj]),
            %error_logger:info_report([{quad_debug, add_obj, Obj}]),
            ?MODULE:loop(State#state{objects=NewObjects});
        {remove_obj, Obj} ->
            NewObjects = lists:delete(Obj, Objects),
            %error_logger:info_report([{quad_debug, remove, Obj}]),
            ?MODULE:loop(State#state{objects=NewObjects});
        {Fun, Args} ->
            %error_logger:info_report([{quad_debug, Fun, Args, Objects}]),
            spawn(?MODULE, send_all, [Objects, Fun, Args]),
            ?MODULE:loop(State)
    end.

send_all([], _Fun, _Args) ->
    ok;

send_all([Obj | Rest], Fun, Args) ->
    obj:async_call(Obj, Fun, Args),
    send_all(Rest, Fun, Args).

