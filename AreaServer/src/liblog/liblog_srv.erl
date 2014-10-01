%%---------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% The server for the player library 'libfaction'. It provides the interface 
%% functions for assigning players to faction.
%% @end
%%---------------------------------------------------------------------
-module(liblog_srv).
-behaviour(gen_server).

%% @headerfile "obj.hrl"
%% @docfile "doc/id.edoc"

% API
-export([
    log/1,
	create_area/0,
	view_log/1,
	clear_log/1,
	add_observer/1,
	remove_observer/1
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

-record(state, {mod, loop_procs=[], state}).

%% @private
start_link(Module) ->
    start_link(?MODULE, Module).

%% @private
start_link(ServerName, Module) ->
    case gen_server:start_link({local, ServerName}, ?MODULE, Module, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, OldPid}} ->
            {ok, OldPid};
        Error ->
            error_logger:error_report([{?MODULE, "start_link/2", Error}])
    end.

%% @private
init(Module) ->
    process_flag(trap_exit, true),
    {ok, State} = Module:init(),
	LoopModules= Module:get_loop_procs(),
	LoopProcs = Module:spawn_loop_procs(LoopModules, []),
    {ok, #state{mod=Module, loop_procs=LoopProcs, state=State}}.

%% @doc
%% @private
handle_call({log, {data, Data}}, _From, #state{mod=Mod} = State) ->
    Result = Mod:log(Data),
    {reply, Result, State};

handle_call(create_area, _From, #state{mod=Mod} = State) ->
    Result = Mod:create_area(),
    {reply, Result, State};

handle_call({view_log, LogType}, _From, #state{mod=Mod} = State) ->
    Result = Mod:view_log(LogType),
    {reply, Result, State};

handle_call({clear_log, {id, Id}}, _From, #state{mod=Mod} = State) ->
    Mod:clear_log(Id, State#state.loop_procs),
	Mod:notify_observers(deleted, Id, State#state.loop_procs),
    {reply, ok, State};

handle_call({add_observer, {pid, Pid}}, _From, #state{mod=Mod} = State) ->
	Mod:add_observer_to_loop_procs(State#state.loop_procs, Pid),
    {reply, ok, State};

handle_call({remove_observer, {pid, Pid}}, _From, #state{mod=Mod} = State) ->
	Mod:remove_observer_from_loop_procs(State#state.loop_procs, Pid),
    {reply, ok, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{unknown_call, Call, State}]),
    {reply, ok, State}.
%% @end

%% @private
handle_info(_Info, State) ->
    {nopreply, State}.

%% @private
handle_cast(Cast, State) ->
	error_logger:info_report({unknown_cast, Cast}),
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{mod=_Mod}) ->
	ok.

create_area() ->
    gen_server:call(?MODULE, create_area).

%%---------------------------------------------------------------------
%% @spec log(Dta) -> ok | {error, Reason}
%% where
%%      Data = any(),
%% @doc
%% @end
%%---------------------------------------------------------------------
log(Data) ->
    gen_server:call(?MODULE, {log, {data, Data}}).

view_log(LogType) ->
    gen_server:call(?MODULE, {view_log, LogType}).

clear_log(Id) ->
    gen_server:call(?MODULE, {clear_log, {id, Id}}).

add_observer(Pid) ->
    gen_server:call(?MODULE, {add_observer, {pid, Pid}}).

remove_observer(Pid) ->
    gen_server:call(?MODULE, {remove_observer, {pid, Pid}}).

