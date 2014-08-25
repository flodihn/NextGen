%%---------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% The server for the player library 'libfaction'. It provides the interface 
%% functions for assigning players to faction.
%% @end
%%---------------------------------------------------------------------
-module(libfaction_srv).
-behaviour(gen_server).

%% @headerfile "obj.hrl"
%% @docfile "doc/id.edoc"

% API
-export([
    assign/0,
	free/1
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
    Module:init(),
    {ok, #state{mod=Module}}.

%% @doc
%% @private
handle_call(assign, _From, #state{mod=Mod} = State) ->
    Result = Mod:assign(),
    {reply, Result, State};

handle_call({free, Faction}, _From, #state{mod=Mod} = State) ->
    Result = Mod:free(Faction),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{Call, State}]),
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
terminate(_Reason, #state{mod=_Mod}) ->
	ok.

%%---------------------------------------------------------------------
%% @spec assign(Id) -> {ok, {faction, Faction}} | {error, Reason}
%% where
%%      Id = binary(),
%%      Faction = 0 | 1 
%% @doc
%% @end
%%---------------------------------------------------------------------
assign() ->
    gen_server:call(?MODULE, assign).


free(Faction) ->
    gen_server:call(?MODULE, {free, Faction}).
