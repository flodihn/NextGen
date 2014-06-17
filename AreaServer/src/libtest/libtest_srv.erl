%%---------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% @end
%%---------------------------------------------------------------------
-module(libtest_srv).
-behaviour(gen_server).

% API
-export([
    start_profiling/0,
    stop_profiling/0
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
    Module:init(),
    {ok, #state{mod=Module}}.

%% @doc
%% @private
handle_call(start_profiling, _From, #state{mod=Mod} = State) ->
    Result = Mod:start_profiling(),
    {reply, Result, State};

handle_call(stop_profiling, _From, #state{mod=Mod} = State) ->
    Result = Mod:stop_profiling(),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{handle_call, Call, State}]),
    {reply, ok, State}.

%% @end

%% @private
handle_info(_Info, State) ->
    {nopreply, State}.

%% @private
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

start_profiling() ->
    gen_server:call(?MODULE, start_profiling).

stop_profiling() ->
    gen_server:call(?MODULE, stop_profiling).

