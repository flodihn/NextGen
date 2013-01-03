-module(conn_dispatcher).


-export([
    dispatch/1
    ]).

dispatch(Socket) ->
    conn_sup:start_conn(Socket).


