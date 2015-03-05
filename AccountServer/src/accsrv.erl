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
    validate/2
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
    module, 
    mod_state
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
            error_logger:error_report([{"start_link/2:", Error}, "Error Here"])
    end.

init([Module]) ->
    {ok, ModState} = Module:init(),
    {ok, #state{module = Module, mod_state = ModState}}.

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
    ModState = State#state.mod_state,
    {ok, Result, NewModState} = Module:create(Name, Email, Pass, ModState),
    {reply, Result, State#state{mod_state = NewModState}};

handle_call({delete, {Name, Password}}, _From, State) ->
    Module = State#state.module,
    Result = Module:delete(Name, Password),
    {reply, Result, State};

handle_call({lookup, Email}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    {ok, Result, NewModState} = Module:lookup(Email, ModState),
    {reply, Result, State#state{mod_state = NewModState}};

handle_call({validate, {Name, Password}}, _From, State) ->
    Module = State#state.module,
    Result = Module:validate(Name, Password),
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

lookup(Email) ->
    gen_server:call(?MODULE, {lookup, Email}).

validate(Name, Pass) ->
    gen_server:call(?MODULE, {validate, {Name, Pass}}).

