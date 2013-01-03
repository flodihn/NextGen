-module(mnesia_char).

-include("char.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([
    init/0,
    stop/0,
    load/1,
    save/4,
    get_list/1
    ]).

-record(char_save, {id, account, name, obj_state}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(char_save, [{attributes, record_info(fields, char_save)},{disc_copies, [node()]}]).

stop() ->
    mnesia:stop().

read(Q) ->
    F = fun() ->
        mnesia:read(Q)
    end,
    mnesia:transaction(F).

write(Q) ->
    F = fun() ->
        mnesia:write(Q)
    end,
    mnesia:transaction(F).

%delete(Q) ->
%    F = fun() ->
%        mnesia:delete(Q)
%    end,
%    mnesia:transaction(F).

%execute(Q) ->
%    F = fun() -> qlc:e(Q) end,
%    {atomic, Val} = mnesia:transaction(F),
%    Val.

load(Id) ->
    case read({char_save, Id}) of 
        {atomic, []} ->
            {ok, StartArea} = application:get_env(start_area),
            {ok, #char{area=StartArea}};
        {atomic, [CharSave]} ->
            {ok, CharSave#char_save.obj_state};
        Result ->
            {error, Result}
    end.

save(Id, Account, Name, ObjState) ->
    CharSave = #char_save{id=Id, account=Account, name=Name, 
        obj_state=ObjState},
    case write(CharSave) of
        {atomic, ok} ->
            {ok, saved};
        Error ->
            error_logger:info_report([{?MODULE, char_save_error, Error}]),
            {error, not_saved}
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
