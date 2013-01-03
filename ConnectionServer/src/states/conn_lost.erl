-module(conn_lost).

% States
-export([
    event/2
    ]).

event(_Event, State) ->
    {next_state, conn_lost, State}.

    
