-module(riak_char).

-include("char.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([
    init/0,
    stop/1,
    save/5,
    get_list/2,
    load/2
    ]).

-record(char_save, {id, account, name, obj_state}).
-record(riak_state, {riak_client_pid}).

init() ->
    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
    {ok, #riak_state{riak_client_pid = Pid}}.

stop(#riak_state{riak_client_pid = Pid}) ->
    riakc_pb_socket:stop(Pid).
%good
save(Id, Account, Name, ObjState, RiakState) ->
    case lookup(Id, RiakState) of
        {ok, avatar_does_not_exist} ->
             NewAvatar = riakc_obj:new(<<"avatars">>, Id, term_to_binary({Account, Name, ObjState})),
             riakc_pb_socket:put(
                RiakState#riak_state.riak_client_pid,
                NewAvatar, 
                [{w, 1}, {dw, 1}, return_body]),
            Response = addid(Account, Id, RiakState),
            error_logger:info_report({"id", Response}),
            {ok, saved, RiakState};
        {ok, {avatar_exists,_}}->
            save(Id, ObjState, RiakState)
    end.
            
%good
save(Id, ObjState, RiakState) ->
    FetchedObj = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatars">>, Id),
    case FetchedObj of 
       {error, notfound} ->
             {ok, could_not_update, RiakState};
        {_, Obj}->
        Value = binary_to_term(riakc_obj:get_value(Obj)),
        NewValue = setelement(3, Value, ObjState),
        UpdatedObj = riakc_obj:update_value(Obj, NewValue),
        riakc_pb_socket:put(
                RiakState#riak_state.riak_client_pid,
                UpdatedObj, 
                [{w, 1}, {dw, 1}, return_body]),
            {ok, saved, RiakState}
    end.
%good
lookup(Id, RiakState)->
    FetchedObj = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatars">>, Id),
    case FetchedObj of
        {error, notfound} ->
            {ok, avatar_does_not_exist};
        {ok, Obj} ->
            Value = binary_to_term(riakc_obj:get_value(Obj)),
            {Acc, _, _} = Value,
%            error_logger:info_report({account_name, Acc}),
            {ok, {avatar_exists, Acc}}
    end.

addid(Account, ID, RiakState) ->
        FetchedObj = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatar_IDs">>, Account),
        case FetchedObj of
            {error, notfound} ->
                IdObj = riakc_obj:new(<<"avatar_IDs">>, Account, term_to_binary({ID})),
                riakc_pb_socket:put(RiakState#riak_state.riak_client_pid, IdObj, [{w, 1}, {dw, 1}, return_body]),
                {ok, id_created};
            {ok, Obj} ->
                Value = binary_to_term(riakc_obj:get_value(Obj)),
                NewValue = erlang:insert_element(1, Value, ID),
                error_logger:info_report({new_value, NewValue}),
                UpdatedObj = riakc_obj:update_value(Obj, NewValue),
                riakc_pb_socket:put(
                    RiakState#riak_state.riak_client_pid,
                    UpdatedObj, 
                    [{w, 1}, {dw, 1}, return_body]),
                {ok, ids_updated}
        end.
                
get_list(Account, RiakState) ->
    FetchedObj = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatar_IDs">>, Account),
    error_logger:info_report(FetchedObj),
        case FetchedObj of
            {error, notfound} ->
                {ok, no_ids_on_account, RiakState};
            {ok, Obj} ->
                Value = binary_to_term(riakc_obj:get_value(Obj)),
                error_logger:info_report({value, Value}),
%                List = tuple_to_list(Value),
%                Result = getid(List,{}, RiakState),
%                error_logger:info_report({get_id_result, Result}),
                {ok, Value, RiakState}
                
        end.
                

%getid(List,Tuple, RiakState) ->
%    [Head|Tail] = List,
%    error_logger:info_report({selected_id, Head}),
%    {ok, Result} = lookup(Head, RiakState),
%    error_logger:info_report({look_up_result, Result}),
%    case Result of
%        avatar_does_not_exist ->
%            {ok, account_flaw};
%        {avatar_exists, Acc} ->
%            NewTuple = erlang:insert_element(1, Tuple, Acc),
%            error_logger:info_report({new_tuple,NewTuple}),
%            getid(Tail, Tuple, RiakState),
%            {ok, NewTuple}
%    end.

load(Id, RiakState) ->
    FetchedObj = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatars">>, Id),
    case FetchedObj of
        {error, notfound} ->
            {ok, avatar_does_not_exist, RiakState};
        {ok, Obj} ->
            {ok, Obj, RiakState}
    end.
