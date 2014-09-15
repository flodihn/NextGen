-module(http_api_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(http_api).

start(_StartType, _StartArgs) ->
    http_api_sup:start_link().

stop(_State) ->
    ok.
