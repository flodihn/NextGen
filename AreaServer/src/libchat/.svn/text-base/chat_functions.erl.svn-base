-module(chat_functions).

% API
-export([
    init/0
    ]).

% handlers
-export([
    say/1
    ]).

init() ->
    areasrv:add_handler(say, libchat).

say({From, say, Msg}) ->
    error_logger:info_report([{From, say, Msg}]),
    {say, From, Msg};

say(Other) ->
    error_logger:info_report([{Other}]).


