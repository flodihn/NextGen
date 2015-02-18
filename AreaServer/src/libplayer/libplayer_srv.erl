%%---------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% The server for the player library 'libplayer'. It provides the interface 
%% functions for creating new and login players.
%% @end
%%---------------------------------------------------------------------
-module(libplayer_srv).
-behaviour(gen_server).

%% @headerfile "obj.hrl"
%% @docfile "doc/id.edoc"

% API
-export([
    create/1,
    login/2,
    save/2
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
handle_call({save, {id, Id}, {obj_state, ObjState}}, _From, 
    #state{mod=Mod} = State) ->
    Result = Mod:save(Id, ObjState),
    {reply, Result, State};

%% @private
handle_call({create, {connection, Conn}}, _From, #state{mod=Mod} = State) ->
    {ok, {pid, Pid}, {id, Id}} = Mod:create(Conn),
    {reply, {char_login, {pid, Pid}, {id, Id}}, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{Call, State}]),
    {reply, ok, State}.
%% @end

%% @private
handle_info(_Info, State) ->
    {nopreply, State}.

handle_cast({login, {conn, Conn}, {id, Id}}, #state{mod=Mod} = State) ->
    Mod:login(Conn, Id),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{mod=Mod}) ->
    error_logger:info_report([{libchar, terminating, cleaning_up}]),
    Mod:unregister_events().

%%---------------------------------------------------------------------
%% @spec create(Conn) -> {ok, {pid, Pid}, {id, Id}} | {error, Reason}
%% where
%%      Conn = pid(),
%%      Pid = pid(),
%%      Id = id()
%% @doc
%% @see libplayer.std_impl:create/1
%% @end
%%---------------------------------------------------------------------
create(Conn) ->
    gen_server:call(?MODULE, {create, {connection, Conn}}).

%%---------------------------------------------------------------------
%% @spec login(State) -> {ok, {pid, Pid}} | {error, Reason}
%% where
%%      State = obj(),
%%      Pid = pid()
%% @doc
%% @see libplayer.std_impl:login/1
%% @end
%%---------------------------------------------------------------------
login(Conn, Id) ->
    gen_server:cast(?MODULE, {login, {conn, Conn}, {id, Id}}).

save(Id, ObjState) ->
    gen_server:call(?MODULE, {save, {id, Id}, {obj_state, ObjState}}).
