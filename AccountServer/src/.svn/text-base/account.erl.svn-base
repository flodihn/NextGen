-module(account).

-include("account.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([
    init/0,
    init_tables/0,
    stop/0,
    ping/0,
    lookup/1,
    delete/2,
    create/3,
    validate/2
    ]).

init_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    Result = mnesia:create_table(account, [{attributes, record_info(fields, account)},{disc_copies, [node()]}]),
    Result.

init() ->
    init_tables(),
    mnesia:start().

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

