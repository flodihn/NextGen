% This event_handler maybe shouldn't be used, remove it?
-module(event_handler).

% API
-export([
    init/0,
    add_handler/2,
    add_handler/3,
    remove_handler/1,
    event/4,
    get_handlers/0
    ]).

init() ->
    ets:new(event_handlers, [named_table]).

load_libs([]) ->
    done;

load_libs([Lib|Tail]) ->
    lib_sup:start(Lib),
    load_libs(Tail).

add_handler(Event, Lib) ->
    ets:insert(event_handlers, {Event, local, Lib}). 

add_handler(Event, Node, Lib) ->
    ets:insert(event_handlers, {Event, Node, Lib}). 

remove_handler(Event) ->
    ets:delete(event_handlers, Event). 

call_event(Event, Node, Lib, []) ->
    case Node of
        local ->
           Lib:event(Event);
        Node ->
            rpc:call(Node, Lib, event, [Event])
    end;

call_event(Event, Node, Lib, Args) ->
    case Node of
        local ->
           Lib:event(Event, Args);
        Node ->
            rpc:call(Node, Lib, event, [Event, Args])
    end.


event(EventId, From, Event, Args) ->
    %error_logger:info_report([{event, From, Event, Args}]),
    case ets:lookup(event_handlers, Event) of
	[{Event, Node, Lib}] ->
	    Result = call_event(Event, Node, Lib, Args),
	    send_result(EventId, From, Result);
	[] ->
	    send_result(EventId, From, {error, {no_such_event, Event}})
    end.
	
get_handlers() ->
    ets:match(event_handlers, '$1').

send_result(EventId, {pid, Pid}, Result) ->
    Pid ! {EventId, {result, Result}}. 

