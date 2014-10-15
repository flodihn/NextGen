-module(libdist_srv).
-behaviour(gen_server).

% API
-export([
    start_log/0,
    create_area/0,
    join_area/0,
    register_obj/2,
    unregister_obj/1,
    get_obj/1,
    next_obj/0,
    next_obj/1,
    find_obj/1,
	get_num_players/0
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

-record(state, {mod, log_proc}).

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
	libdist_balancer:init(),
    {ok, #state{mod=Module}}.

handle_call(start_log, _From, #state{mod=Mod} = State) ->
    Result = Mod:start_log(),
    {reply, Result, State};

handle_call(create_area, _From, #state{mod=Mod} = State) ->
    Result = Mod:create_area(),
    {reply, Result, State};

handle_call(join_area, _From, #state{mod=Mod} = State) ->
    Mod:join_area(),
    {reply, ok, State};

handle_call({register_obj, AreaId, Pid}, _From, #state{mod=Mod} = State) ->
    Reply = Mod:register_obj(AreaId, Pid),
    {reply, Reply, State};

handle_call({unregister_obj, AreaId}, _From, #state{mod=Mod} = State) ->
    Reply = Mod:unregister_obj(AreaId),
    {reply, Reply, State};

handle_call({get_obj, AreaId}, _From, #state{mod=Mod} = State) ->
    Reply = Mod:get_obj(AreaId),
    {reply, Reply, State};

handle_call(next_obj, _From, #state{mod=Mod} = State) ->
    Reply = Mod:next_obj(),
    {reply, Reply, State};

handle_call({next_obj, AreaId}, _From, #state{mod=Mod} = State) ->
    Reply = Mod:next_obj(AreaId),
    {reply, Reply, State};

handle_call({find_obj, AreaId}, _From, #state{mod=Mod} = State) ->
    Reply = Mod:find_obj(AreaId),
    {reply, Reply, State};

handle_call(get_num_player, _From, #state{mod=Mod} = State) ->
    Reply = Mod:get_num_players(),
    {reply, Reply, State};

handle_call(_Call, _From, State) ->
    %error_logger:info_report([{?MODULE, unknown_call, Call}]),
    %error_logger:info_report([{?MODULE, self()}]),
    {reply, ok, State}.

handle_info(_Info, State) ->
    {nopreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

start_log() ->
    gen_server:call(?MODULE, start_log).

create_area() ->
    gen_server:call(?MODULE, create_area).

join_area() ->
    gen_server:call(?MODULE, join_area).

register_obj(AreaId, Pid) ->
    gen_server:call(?MODULE, {register_obj, AreaId, Pid}).

unregister_obj(AreaId) ->
    gen_server:call(?MODULE, {register_obj, AreaId}).

get_obj(AreaId) ->
    gen_server:call(?MODULE, {get_obj, AreaId}).

next_obj() ->
    gen_server:call(?MODULE, next_obj).

next_obj(AreaId) ->
    gen_server:call(?MODULE, {next_obj, AreaId}).

find_obj(Id) ->
    gen_server:call(?MODULE, {find_obj, Id}).

get_num_players() ->
    gen_server:call(?MODULE, get_num_players).
