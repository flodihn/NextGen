%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the server for the standard library 'libstd'.
%% The module provides functions for creating an area, generating unique
%% ids, upgrading modules, registering and lookups of objects.
%% @end
%%----------------------------------------------------------------------

-module(libstd.srv).
-behaviour(gen_server).

-import(gen_server).
-import(error_logger).

% API
-export([
    event/1,
    event/2
    ]).

%external exports
-export([
    start_link/1,
    start_link/2,
    create_area/0,
    upgrade/1,
    area_name/0,
    area_name/1,
    area_name/2,
    get_obj/1,
    next_obj/0,
    next_obj/1,
    register_obj/2,
    unregister_obj/1,
    generate_id/1,
    get_pid/1,
    get_all_pids/0,
    monsrv_rpc/2,
    monsrv_rpc/3
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

%%----------------------------------------------------------------------
%% @doc
%% @spec init(Module) -> any()
%% @private
%% @end
%%----------------------------------------------------------------------
init(Module) ->
    Module:init(),
    {ok, #state{mod=Module}}.

%%----------------------------------------------------------------------
%% @doc
%% @private
%%----------------------------------------------------------------------
handle_call({upgrade, Module}, _From, #state{mod=Mod} = State) ->
    Result = Mod:upgrade(Module),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(create_area, _From, #state{mod=Mod} = State) ->
    Result = Mod:create_area(),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(area_name, _From, #state{mod=Mod} = State) ->
    Result = Mod:area_name(),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({area_name, NodeOrExtra}, _From, #state{mod=Mod} = State) ->
    Result = Mod:area_name(NodeOrExtra),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({area_name, Node, Extra}, _From, #state{mod=Mod} = State) ->
    Result = Mod:area_name(Node, Extra),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({get_obj, AreaId}, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_obj(AreaId),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(next_obj, _From, #state{mod=Mod} = State) ->
    Result = Mod:next_obj(),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({next_obj, AreaId}, _From, #state{mod=Mod} = State) ->
    Result = Mod:next_obj(AreaId),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({generate_id, Type}, _From, #state{mod=Mod} = State) ->
    Result = Mod:generate_id(Type),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({get_pid, Id}, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_pid(Id),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(get_all_pids, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_all_pids(),
    {reply, Result, State};

handle_call({monsrv_rpc, RpcMod, Fun, Args}, _From, 
    #state{mod=Mod} = State) ->
    Result = Mod:monsrv_rpc(RpcMod, Fun, Args),
    {reply, Result, State};

%handle_call({reg_local, {AreaId, Pid}}, _From, #state{mod=Mod} = State) ->
%    Result = Mod:reg_local(AreaId, Pid),
%    %error_logger:info_report([{?MODULE, self()}]),
%    {reply, Result, State};

%handle_call({unreg_local, AreaId}, _From, #state{mod=Mod} = State) ->
%    Result = Mod:unreg_local(AreaId),
%    %error_logger:info_report([{?MODULE, self()}]),
%    {reply, Result, State};

handle_call({event, Event}, _From, #state{mod=Mod} = State) ->
    Result = apply(Mod, Event, []),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({event, Event, Args}, _From, #state{mod=Mod} = State) ->
    Result = apply(Mod, Event, Args),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{?MODULE, unknown_call, Call}]),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% @end
%%----------------------------------------------------------------------

%% @private
handle_info(_Info, State) ->
    {nopreply, State}.

handle_cast({register_obj, {Ref, Pid}},#state{mod=Mod} = State) ->
    Mod:register_obj(Ref, Pid),
    %error_logger:info_report([{?MODULE, self()}]),
    {noreply, State};

handle_cast({unregister_obj, Id}, #state{mod=Mod} = State) ->
    Mod:unregister_obj(Id),
    %error_logger:info_report([{?MODULE, self()}]),
    {noreply, State};


%% @private
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% @doc
%% @spec event(Event) -> any()
%% @deprecated Warning, this function will be removed.
%% @end
%%----------------------------------------------------------------------
event(Event) ->
    gen_server:call(?MODULE, {event, Event}).

%%----------------------------------------------------------------------
%% @doc
%% @spec event(Event, Args) -> any()
%% @deprecated Warning, this function will be removed.
%% @end
%%----------------------------------------------------------------------
event(Event, Args) ->
    gen_server:call(?MODULE, {event, Event, Args}).

%%----------------------------------------------------------------------
%% @doc
%% @spec upgrade(Module) -> any()
%% @see libstd.std_impl:upgrade/1
%% @end
%%----------------------------------------------------------------------
upgrade(Module) ->
	gen_server:call(?MODULE, {upgrade, Module}).

%%----------------------------------------------------------------------
%% @doc
%% @spec create_area() -> any()
%% @see libstd.std_impl:create_area/0
%% @end
%%----------------------------------------------------------------------
create_area() ->
	gen_server:call(?MODULE, create_area).

%%----------------------------------------------------------------------
%% @doc
%% @spec area_name() -> any()
%% @see libstd.std_impl:area_name/0
%% @end
%%----------------------------------------------------------------------
area_name() ->
	gen_server:call(?MODULE, area_name).

%%----------------------------------------------------------------------
%% @doc
%% @spec area_name(NodeOrExtra) -> any()
%% @see libstd.std_impl:area_name/1
%% @end
%%----------------------------------------------------------------------
area_name(NodeOrExtra) ->
	gen_server:call(?MODULE, {area_name, NodeOrExtra}).

%%----------------------------------------------------------------------
%% @doc
%% @spec area_name(Node, Extra) -> any()
%% @see libstd.std_impl:area_name/2
%% @end
%%----------------------------------------------------------------------
area_name(Node, Extra) ->
	gen_server:call(?MODULE, {area_name, Node, Extra}).

%%----------------------------------------------------------------------
%% @doc
%% @spec next_obj() -> any()
%% @see libstd.std_impl:next_obj/0
%% @end
%%----------------------------------------------------------------------
next_obj() ->
	gen_server:call(?MODULE, next_obj).

%%----------------------------------------------------------------------
%% @doc
%% @spec next_obj(AreaId) -> any()
%% @see libstd.std_impl:next_obj/1
%% @end
%%----------------------------------------------------------------------
next_obj(Id) ->
	gen_server:call(?MODULE, {next_obj, Id}).

%%----------------------------------------------------------------------
%% @doc
%% @spec get_obj(Next) -> any()
%% @see libstd.std_impl:get_obj/1
%% @end
%%----------------------------------------------------------------------
get_obj(Next) ->
	gen_server:call(?MODULE, {get_obj, Next}).

%%----------------------------------------------------------------------
%% @doc
%% @spec register_obj(Ref, Pid) -> any()
%% @see libstd.std_impl:register_obj/1
%% @end
%%----------------------------------------------------------------------
register_obj(Id, Pid) ->
    gen_server:cast(?MODULE, {register_obj, {Id, Pid}}).

%%----------------------------------------------------------------------
%% @doc
%% @spec unregister_obj(Id) -> any()
%% @see libstd.std_impl:unregister_obj/1
%% @end
%%----------------------------------------------------------------------
unregister_obj(Id) ->
    gen_server:cast(?MODULE, {unregister_obj, Id}).

%%----------------------------------------------------------------------
%% @doc
%% @spec generate_id(Type) -> any()
%% @see libstd.std_impl:generate_id/1
%% @end
%%----------------------------------------------------------------------
generate_id(Type) ->
    gen_server:call(?MODULE, {generate_id, Type}).

%%----------------------------------------------------------------------
%% @doc
%% @spec get_pid(Id) -> any()
%% @see libstd.std_impl:get_pid/1
%% @end
%%----------------------------------------------------------------------
get_pid(Id) ->
    gen_server:call(?MODULE, {get_pid, Id}).


get_all_pids() ->
    gen_server:call(?MODULE, get_all_pids).

monsrv_rpc(Mod, Fun) ->
    monsrv_rpc(Mod, Fun, []).

monsrv_rpc(Mod, Fun, Args) ->
    gen_server:call(?MODULE, {monsrv_rpc, Mod, Fun, Args}).
    
