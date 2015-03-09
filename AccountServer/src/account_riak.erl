-module(account_riak).

-include("account.hrl").

-export([
    init/0,
    stop/1,
    ping/0,
    lookup/2,
    delete/3,
    create/4,
    validate/3
    ]).

-record(riak_state, {riak_client_pid}).

init() ->
    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
    {ok, #riak_state{riak_client_pid = Pid}}.
    %error_logger:info_report("Database Initialized", Pid).

stop(#riak_state{riak_client_pid = Pid}) ->
    riakc_pb_socket:stop(Pid),
    error_logger:info_report("Database Stopped").
    
ping() ->
    alive.

create(Name, Email, Passwd, RiakState) ->
    NewAccount = riakc_obj:new(<<"accounts">>, Email, term_to_binary({Name, Passwd})),
    riakc_pb_socket:put(
        RiakState#riak_state.riak_client_pid,
        NewAccount, 
        [{w, 1}, {dw, 1}, return_body]),
%    case lookup(Name) of 
%        {ok, false} ->
%            Row = #account{name=Name, email=Email, passwd=Passwd, characters=[]},
%            F = fun() ->
%                mnesia:write(Row)
%            end,
%            case mnesia:transaction(F) of
%                {atomic, ok} ->
%                    {ok, account_created};
%               {error, Reason} ->
%                    {error, Reason}
%            end;
%        {ok, true} ->
%            {error, account_name_exists};
%        {aborted, {node_not_running, _Node}} ->
%            {error, node_not_running}
%    end.
    {ok, lookup(Email, RiakState), RiakState}.

lookup(Email, RiakState) ->
%    case read({account, Name}) of 
%        {atomic, []} ->
%            {ok, false};
%        {atomic, _Record} ->
%            {ok, true};
%        Result ->
%            {error, Result}
%    end.

 FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.riak_client_pid,
        <<"accounts">>,Email),

    case FetchedObj of
        {error, notfound} -> 
            {ok, false, RiakState};
        {_,_} -> 
            {ok,true, RiakState}
    end.

%    Value = riakc_obj:get_value(FetchedObj),
%    {Name, Passwd} = binary_to_term(Value),
%    error_logger:info_report({Name, Passwd}),

delete(Email, Pass, RiakState) ->
%    case read({account, Name}) of 
%        {atomic, []} ->
%           {error, wrong_username_or_password};
%       {atomic, [{account, Name, _Mail, Pass, _Characters}]} ->
%            error_logger:info_report([{"Deleted account:", Name}]),
%            delete({account, Name});
%        {atomic, [{account, Name, _Mail, _Pass, _Characters}]} ->
%            {error, wrong_username_or_password};
%        {aborted, {node_not_running, _Node}} ->
%            {error, node_not_running}
%    end.

    {ok, FetchedObj} = riakc_pb_socket:get(
        RiakState#riak_state.riak_client_pid,
        <<"accounts">>,Email),

    Value = binary_to_term(riakc_obj:get_value(FetchedObj)),

    case Value of
        {_, Pass} -> 
            riakc_pb_socket:delete(
                RiakState#riak_state.riak_client_pid,
                <<"accounts">>,
                Email),
            {ok, account_deleted, RiakState};
        {_,_} -> 
            {ok, wrong_password, RiakState}
    end.


validate(Name, Pass, RiakState) ->
%    case read({account, Name}) of 
%        {atomic, [{account, Name, _Mail, Pass, _Characters}]} ->
%             {ok, match};
%        {atomic, Error} ->
%            error_logger:info_report([{validate, Name, Pass, Error}]),
%            {error, wrong_username_or_password};
%        {aborted, {node_not_running, _Node}} ->
%            {error, node_not_running}
%    end.
    {ok, RiakState}.

