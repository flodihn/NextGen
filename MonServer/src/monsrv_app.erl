-module(monsrv_app).
-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
    ]).

start() ->
    application:start(monsrv).

start(_Type, StartArgs) ->
    monsrv_sup:start_link(StartArgs).

stop(_State) ->
    ok.
