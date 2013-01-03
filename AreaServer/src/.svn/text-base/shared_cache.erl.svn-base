-module(shared_cache).

-export([
    init/0,
    store/2,
    retr/1
    ]).

init() ->
    ets:new(?MODULE, [named_table, public]).


store(Key, Val) ->
    ets:insert(?MODULE, {Key, Val}).

retr(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Val}] ->
            Val;
        [] ->
            undefined
    end.


