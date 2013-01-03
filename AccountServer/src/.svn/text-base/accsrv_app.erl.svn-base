-module(accsrv_app).
-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
    ]).

start() ->
    application:start(accsrv).

start(_Type, StartArgs) ->
    accsrv_sup:start_link(StartArgs).

stop(_State) ->
    ok.
