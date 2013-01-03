-module(charsrv_app).
-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
    ]).

start() ->
    application:start(charsrv).

start(_Type, StartArgs) ->
    charsrv_sup:start_link(StartArgs).

stop(_State) ->
    ok.
