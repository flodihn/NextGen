-module(account_riak).

-include("account.hrl").

-export([
    init/0,
    stop/0,
    ping/0,
    lookup/1,
    delete/2,
    create/3,
    validate/2
    ]).

-record(riak_state, {riak_client_pid}).

init() ->
    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
    {ok, #riak_state{riak_client_pid = Pid}}.

stop() ->
    mnesia:stop().

read(Q) ->
    F = fun() ->
        mnesia:read(Q)
    end,
    mnesia:transaction(F).

delete(Q) ->
    F = fun() ->
        mnesia:delete(Q)
    end,
    mnesia:transaction(F).

ping() ->
    alive.

create(Name, Email, Passwd) ->
    case lookup(Name) of 
        {ok, false} ->
            Row = #account{name=Name, email=Email, passwd=Passwd, characters=[]},
            F = fun() ->
                mnesia:write(Row)
            end,
            case mnesia:transaction(F) of
                {atomic, ok} ->
                    {ok, account_created};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, true} ->
            {error, account_name_exists};
        {aborted, {node_not_running, _Node}} ->
            {error, node_not_running}
    end.

lookup(Name) ->
    case read({account, Name}) of 
        {atomic, []} ->
            {ok, false};
        {atomic, _Record} ->
            {ok, true};
        Result ->
            {error, Result}
    end.

delete(Name, Pass) ->
    case read({account, Name}) of 
        {atomic, []} ->
            {error, wrong_username_or_password};
        {atomic, [{account, Name, _Mail, Pass, _Characters}]} ->
            error_logger:info_report([{"Deleted account:", Name}]),
            delete({account, Name});
        {atomic, [{account, Name, _Mail, _Pass, _Characters}]} ->
            {error, wrong_username_or_password};
        {aborted, {node_not_running, _Node}} ->
            {error, node_not_running}
    end.

validate(Name, Pass) ->
    case read({account, Name}) of 
        {atomic, [{account, Name, _Mail, Pass, _Characters}]} ->
             {ok, match};
        {atomic, Error} ->
            error_logger:info_report([{validate, Name, Pass, Error}]),
            {error, wrong_username_or_password};
        {aborted, {node_not_running, _Node}} ->
            {error, node_not_running}
    end.

