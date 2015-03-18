-module(riak_char).

-include("char.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([
    init/0,
    stop/1,
    save/5,
    get_list/1
    ]).

-record(char_save, {id, account, name, obj_state}).
-record(riak_state, {riak_client_pid}).

init() ->
    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
    {ok, #riak_state{riak_client_pid = Pid}}.

stop(#riak_state{riak_client_pid = Pid}) ->
    riakc_pb_socket:stop(Pid).


%delete(Q) ->
%    F = fun() ->
%        mnesia:delete(Q)
%    end,
%    mnesia:transaction(F).

%execute(Q) ->
%    F = fun() -> qlc:e(Q) end,
%    {atomic, Val} = mnesia:transaction(F),
%    Val.    

%load(Id) ->
%    case read({char_save, Id}) of 
%        {atomic, []} ->
%            {ok, StartArea} = application:get_env(start_area),
%            {ok, #char{area=StartArea}};
%        {atomic, [CharSave]} ->
%            {ok, CharSave#char_save.obj_state};
%        Result ->
%            {error, Result}
%    end.

save(Id, Account, Name, ObjState, RiakState) ->
    case lookup(Id, RiakState) of
        {ok, avatar_does_not_exist} ->
             NewAvatar = riakc_obj:new(<<"avatars">>, Id, term_to_binary({Account, Name, ObjState})),
             riakc_pb_socket:put(
                RiakState#riak_state.riak_client_pid,
                NewAvatar, 
                [{w, 1}, {dw, 1}, return_body]),
            {ok, saved, RiakState};
        {ok, avatar_exists}->
            save(Id, ObjState, RiakState)
    end.
            

save(Id, ObjState, RiakState) ->
    {ok, FetchedObj} = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatars">>, Id),
    Value = binary_to_term(riakc_obj:get_value(FetchedObj)),
    NewValue = setelement(3, Value, ObjState),
    UpdatedObj = riakc_obj:update_value(FetchedObj, NewValue),
    riakc_pb_socket:put(
            RiakState#riak_state.riak_client_pid,
            UpdatedObj, 
            [{w, 1}, {dw, 1}, return_body]),
        {ok, saved, RiakState}.
    
lookup(Id, RiakState)->
    FetchedObj = riakc_pb_socket:get(RiakState#riak_state.riak_client_pid, <<"avatars">>, Id),
    case FetchedObj of
        {error, notfound} ->
            {ok, avatar_does_not_exist};
        {ok, _} ->
            {ok, avatar_exists}
    end.
        
get_list(Account) ->
    % This is same as:
    % SELECT id from char_save where account=Account
    do(qlc:q([{X#char_save.id, X#char_save.name} || 
        X <- mnesia:table(char_save),
        X#char_save.account == Account])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
