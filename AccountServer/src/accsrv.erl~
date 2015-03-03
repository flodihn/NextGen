-module(accsrv).
-behaviour(gen_server).

% External exports
-export([
    start_link/1,
    start_link/2
    ]).

% Api functions
-export([
    create/3,
    delete/2,
    lookup/1,
    validate/2,
    init_tables/0
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
-record(state, {
    module
    }).

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
    Module:init(),
    {ok, #state{module = Module}}.

stop(Server) ->
    gen_server:call(Server, stop).

handle_info(Info, State) ->
    error_logger:info_report([{"Info:", Info}]),
    {nopreply, State}.

handle_cast(Cast, State) ->
    error_logger:info_report([{"Cast:", Cast}]),
    {nopreply, State}.


handle_call({create, {Name, Email, Pass}}, _From, State) ->
    Module = State#state.module,
    Result = Module:create(Name, Email, Pass),
    {reply, Result, State};

handle_call({delete, {Name, Password}}, _From, State) ->
    Module = State#state.module,
    Result = Module:delete(Name, Password),
    {reply, Result, State};

handle_call({lookup, Name}, _From, State) ->
    Module = State#state.module,
    Result = Module:lookup(Name),
    {reply, Result, State};

handle_call({validate, {Name, Password}}, _From, State) ->
    Module = State#state.module,
    Result = Module:validate(Name, Password),
    {reply, Result, State};

handle_call(init_tables, _From,  State) ->
    Module = State#state.module,
    Result = Module:init_tables(),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, requested, State}.

code_change(OldVsn, State, Extra) ->
    error_logger:info_report([{"code_change:", OldVsn, Extra}]),
    {ok, State}.

terminate(Reason, State) ->
    Module = State#state.module,
    Module:stop(),
    error_logger:info_report([{"terminate:", Reason}]).

create(Name, Email, Pass) ->
    gen_server:call(?MODULE, {create, {Name, Email, Pass}}).

delete(Name, Pass) ->
    gen_server:call(?MODULE, {create, {Name, Pass}}).

lookup(Name) ->
    gen_server:call(?MODULE, {lookup, Name}).

validate(Name, Pass) ->
    gen_server:call(?MODULE, {validate, {Name, Pass}}).

init_tables() ->
    gen_server:call(?MODULE, init_tables).

