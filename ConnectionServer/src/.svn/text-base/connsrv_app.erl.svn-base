-module(connsrv_app).
-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
    ]).

start() ->
    application:start(connsrv).

start(_Type, StartArgs) ->
    connsrv_sup:start_link(StartArgs).

stop(_State) ->
    ok.
