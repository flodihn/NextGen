-module(areasrv).
-behaviour(gen_server).

% API
-export([
    add_handler/2,
    add_handler/3,
    remove_handler/1,
	event/1,
	event/2,
	event/3,
    get_handlers/0
    ]).

%external exports
-export([
    start_link/1,
    start_link/2
    ]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
    ]).

-record(state, {mod}).


start_link(Module) ->
    start_link(?MODULE, Module).

start_link(ServerName, Module) ->
    case gen_server:start_link({local, ServerName}, ?MODULE, Module, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}} ->
            {ok, OldPid};
        Error ->
            error_logger:error_report([{?MODULE, "start_link/2", Error}])
    end.

init(Module) ->
    %Test = #obj_state{},
    %io:format("Test: ~p~n", [Test]),
	Module:init(),
    {ok, #state{mod=Module}}.

handle_call({add_handler, Event, Lib}, _From, 
    #state{mod=Mod} = State) ->
    Result = Mod:add_handler(Event, Lib),
    {reply, Result, State};

handle_call({add_handler, Event, Node, Lib}, _From, 
    #state{mod=Mod} = State) ->
    Result = Mod:add_handler(Event, Node, Lib),
    {reply, Result, State};

handle_call({remove_handler, Event}, _From, 
    #state{mod=Mod} = State) ->
    Result = Mod:remove_handler(Event),
    {reply, Result, State};


handle_call(get_handlers, _From, #state{mod=Mod} = State) ->
    Info = Mod:get_handlers(),
    {reply, Info, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{unknown_call, Call, State}]),
    {reply, ok, State}.

handle_info(Info, State) ->
    error_logger:info_report([{unknown_info, Info, State}]),
    {noreply, State}.

handle_cast({event, EventId, From, Event, Args}, #state{mod=Mod} = State) ->
    spawn(Mod, event, [EventId, From, Event, Args]),
    {noreply, State};

handle_cast(Cast, State) ->
    error_logger:info_report([{unknown_cast, Cast, State}]),
    {nopreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

create_event_id() ->
    {event_id, make_ref()}.

add_handler(Event, Node, Lib) ->
    gen_server:call(?MODULE, {add_handler, Event, Node, Lib}).

add_handler(Event, Lib) ->
    gen_server:call(?MODULE, {add_handler, Event, Lib}).

remove_handler(Event) ->
    gen_server:call(?MODULE, {remove_handler, Event}).

event(Event) ->
    event({pid, self()}, Event, []).

event(Event, Args) ->
    %EventId = create_event_id(),
    %case Event of
    %    {Function, Args} ->
    %        gen_server:cast(?MODULE, {event, EventId, From, Function, 
    %            Args});
    %    Function ->
    %        gen_server:cast(?MODULE, {event, EventId, From, Function})
    %end,
    %EventId.
	event({pid, self()}, Event, Args).

event(From, Event, Args) ->
    EventId = create_event_id(),
	gen_server:cast(?MODULE, {event, EventId, From, Event, Args}),
    EventId.

get_handlers() ->
    gen_server:call(?MODULE, get_handlers).

