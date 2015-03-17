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

write(AvatarData, RiakState) ->
    Id = AvatarData#char_save.id,
    Account = AvatarData#char_save.account,
    Name = AvatarData#char_save.name,
    ObjState = AvatarData#char_save.obj_state,
    NewAvatar = riakc_obj:new(<<"avatars">>, Id, term_to_binary({Account, Name, ObjState})),
    riakc_pb_socket:put(
        RiakState#riak_state.riak_client_pid,
        NewAvatar, 
        [{w, 1}, {dw, 1}, return_body]),
    {ok, saved, RiakState}.
    

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
    CharSave = #char_save{id=Id, account=Account, name=Name, 
        obj_state=ObjState},
    case write(CharSave, RiakState) of
        {atomic, ok, NewRiakState} ->
            {ok, saved, NewRiakState};
        Error ->
            error_logger:info_report([{?MODULE, char_save_error, Error}]),
            {error, not_saved, RiakState}
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
