-module(libchat).
-behaviour(gen_server).

% API
-export([
    event/1,
    event/2
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

-record(state, {mod, tab}).

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
    Module:init(),
    {ok, #state{mod=Module}}.

handle_call({event, Event}, _From, #state{mod=Mod} = State) ->
    Result = Mod:Event(),
    error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({event, Event, Args}, _From, #state{mod=Mod} = State) ->
    Result = Mod:Event(Args),
    error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{?MODULE, unknown_call, Call}]),
    error_logger:info_report([{?MODULE, self()}]),
    {reply, ok, State}.

handle_info(_Info, State) ->
    {nopreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

event(Event) ->
    gen_server:call(?MODULE, {event, Event}).

event(Event, Args) ->
    gen_server:call(?MODULE, {event, Event, Args}).
