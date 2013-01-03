%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the server for the environment library 'libenv'.
%% The library stores the skybox and the terrain of the area.
%% @end
%%----------------------------------------------------------------------
-module(libenv.srv).
-behaviour(gen_server).

-import(gen_server).
-import(error_logger).

% API
-export([
    create_area/0,
    join_area/1,
    set_terrain/1,
    get_terrain/0,
    set_skybox/1,
    get_skybox/0
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


%% @doc
%% @private
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
%% @end

%% @private
init(Module) ->
    Module:init(),
    {ok, #state{mod=Module}}.

%% @doc
%% @private
handle_call(create_area, _From, #state{mod=Mod} = State) ->
    Result = Mod:create_area(),
    {reply, Result, State};

handle_call({join_area, Node}, _From, #state{mod=Mod} = State) ->
    Result = Mod:join_area(Node),
    {reply, Result, State};

handle_call({set_terrain, Terrain}, _From, #state{mod=Mod} = State) ->
    Result = Mod:set_terrain(Terrain),
    {reply, Result, State};

handle_call(get_terrain, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_terrain(),
    {reply, Result, State};

handle_call({set_skybox, Skybox}, _From, #state{mod=Mod} = State) ->
    Result = Mod:set_skybox(Skybox),
    {reply, Result, State};

handle_call(get_skybox, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_skybox(),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{?MODULE, unknown_call, Call}]),
    error_logger:info_report([{?MODULE, self()}]),
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

%% @end

%%----------------------------------------------------------------------
%% @spec create_area() -> ok
%% @doc
%% @see libenv.simple_env:create_area/0
%% @end
%%----------------------------------------------------------------------
create_area() ->
    gen_server:call(?MODULE, create_area).

join_area(Node) ->
    gen_server:call(?MODULE, {join_area, Node}).
%%----------------------------------------------------------------------
%% @spec set_terrain(Terrain) -> ok
%% @doc
%% @see libenv.simple_env:set_terrain/1
%% @end
%%----------------------------------------------------------------------
set_terrain(Terrain) ->
    gen_server:call(?MODULE, {set_terrain, Terrain}).

%%----------------------------------------------------------------------
%% @spec get_terrain() -> ok
%% @doc
%% @see libenv.simple_env:get_terrain/0
%% @end
%%----------------------------------------------------------------------
get_terrain() ->
    gen_server:call(?MODULE, get_terrain).

%%----------------------------------------------------------------------
%% @spec set_skybox(Skybox) -> ok
%% @doc
%% @see libenv.simple_env:set_skybox/0
%% @end
%%----------------------------------------------------------------------
set_skybox(Skybox) ->
    gen_server:call(?MODULE, {set_skybox, Skybox}).

%%----------------------------------------------------------------------
%% @spec get_skybox() -> ok
%% @doc
%% @see libenv.simple_env:get_skybox/0
%% @end
%%----------------------------------------------------------------------
get_skybox() ->
    gen_server:call(?MODULE, get_skybox).
