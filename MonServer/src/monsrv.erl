-module(monsrv).
-behaviour(gen_server).

% External exports
-export([
    start_link/1,
    start_link/2
    ]).

% Api functions
-export([
    start_log/0,
    start_log/1,
    stop_log/0,
    log/1,
    log/0,
    register_node/1,
    get_area/1
    ]).
    
% Gen server callbacks
-export([
    init/1,
    stop/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

% Server state
-include("state.hrl").

start_link(Module) ->
    start_link(?MODULE, Module).

start_link(ServerName, Module) ->
    case gen_server:start_link({local, ServerName}, ?MODULE, 
    [Module], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}} ->
            {ok, OldPid};
        Error ->
            error_logger:error_report([{"start_link/2:", Error}])
    end.

init([Module]) ->
    NewState = Module:init(#state{}),
    {ok, NewState#state{module = Module}}.

stop(Server) ->
    gen_server:call(Server, stop).

handle_info(Info, State) ->
    error_logger:info_report([{"Info:", Info}]),
    {nopreply, State}.

handle_cast(Cast, State) ->
    error_logger:info_report([{"Cast:", Cast}]),
    {nopreply, State}.

handle_call(log, _From,  #state{module=Module} = State) ->
    Result = Module:log(State),
    {reply, Result, State};

handle_call({log, Line}, _From,  
    #state{module=Module, logfile=LogFile} = State) ->
    Result = Module:log(Line, LogFile),
    {reply, Result, State};

handle_call({start_log, {interval, Interval}}, _From,  
        #state{module=Module} = State) ->
    Result = Module:start(Interval),
    {reply, Result, State};

handle_call({register_node, Node}, _From, #state{module=Module} = State) ->
    Result = Module:register_node(Node),
    {reply, Result, State};

handle_call({get_area, Area}, _From, #state{module=Module} = State) ->
    Result = Module:get_area(Area),
    {reply, Result, State};

handle_call(stop_log, _From, #state{module=Module} = State) ->
    Result = Module:stop(),
    {reply, Result, State}.

code_change(OldVsn, State, Extra) ->
    error_logger:info_report([{"code_change:", OldVsn, Extra}]),
    {ok, State}.

terminate(Reason, State) ->
    Module = State#state.module,
    Module:cleanup(State),
    error_logger:info_report([{"terminate:", Reason}]).

start_log() ->
    start_log(1000).

start_log(Interval) ->
    gen_server:call(?MODULE, {start_log, {interval, Interval}}).

stop_log() ->
    gen_server:call(?MODULE, stop_log).

log() ->
    gen_server:call(?MODULE, log).

log(Line) ->
    gen_server:call(?MODULE, {log, Line}).

register_node(Node) ->
    gen_server:call(?MODULE, {register_node, Node}).

get_area(Area) ->
    gen_server:call(?MODULE, {get_area, Area}).

