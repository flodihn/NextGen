-module(charsrv).
-behaviour(gen_server).

% External exports
-export([
    start_link/1,
    start_link/2
    ]).

% Api functions
-export([
    load/1,
    save/4,
    get_list/1
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

handle_call({load, {char_id, Id}}, _From, State) ->
    Module = State#state.module,
    Result = Module:load(Id),
    {reply, Result, State};

handle_call({save, {id, Id}, {account, Account}, {name, Name}, 
    {obj_state, ObjState}}, _From, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    {ok, Result, NewModState} = Module:save(Id, Account, Name, ObjState, ModState),
    {reply, Result, State#state{mod_state = NewModState}};

handle_call({get_list, {account, Account}}, _From, State) ->
    Module = State#state.module,
    Result = Module:get_list(Account),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, requested, State}.

code_change(OldVsn, State, Extra) ->
    error_logger:info_report([{"code_change:", OldVsn, Extra}]),
    {ok, State}.

terminate(Reason, State) ->
    Module = State#state.module,
    ModState = State#state.mod_state,
    Module:stop(ModState),
    error_logger:info_report([{"terminate:", Reason}]).

load(Id) ->
    gen_server:call(?MODULE, {load , {char_id, Id}}).

save(Id, Account, Name, ObjState) ->
    gen_server:call(?MODULE, {save, {id, Id}, {account, Account}, 
        {name, Name}, {obj_state, ObjState}}).

get_list(Account) ->
    gen_server:call(?MODULE, {get_list, {account, Account}}).
