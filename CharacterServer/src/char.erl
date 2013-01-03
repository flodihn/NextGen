-module(char).

-include("char.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([
    init/0,
    init_tables/0,
    stop/0,
    retreive_char/1,
    save_char/1
    ]).

init_tables() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(char, [{attributes, record_info(fields, char)},{disc_copies, [node()]}]).

init() ->
    error_logger:info_report([{char, init}]),
    init_tables().

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

retreive_char({char_id, Id}) ->
    case read({char, Id}) of 
        {atomic, []} ->
            {ok, StartArea} = application:get_env(start_area),
            {ok, #char{area=StartArea}};
        {atomic, Record} ->
            {ok, Record};
        Result ->
            {error, Result}
    end.

save_char(Char) ->
    case write(Char) of
        {atomic, ok} ->
            {ok, saved};
        Error ->
            error_logger:info_report([{?MODULE, char_save_error, Error}]),
            {error, not_saved}
    end.

