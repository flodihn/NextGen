-module(event).

% API
-export([
	gather/1
    ]).

gather(EventId) ->
    receive 
        {EventId, {result, Result}} ->
            Result
    after 60000 ->
        {error, timeout}
    end.
