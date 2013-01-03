% This module is not used, perhapse remove it?
-module(event).

% API
-export([
	gather/1
    ]).

gather({event_id, EventId}) ->
    receive 
        {{event_id, EventId}, {result, Result}} ->
            Result
    after 1000 ->
        {error, timeout}
    end.




    
