-module(libsave_srv).
-behaviour(gen_server).

% API
-export([
    create_area/0,
    save/2,
    restore/1,
    next/0,
    next/1,
    restore_all/0
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
    {ok, #state{mod=Module}}.

handle_call(create_area, _From, #state{mod=Mod} = State) ->
    Result = Mod:create_area(),
    {reply, Result, State};

handle_call({save, Id, ObjState}, _From, #state{mod=Mod} = State) ->
    Result = Mod:save(Id, ObjState),
    {reply, Result, State};

handle_call({restore, Id}, _From, #state{mod=Mod} = State) ->
    Result = Mod:restore(Id),
    {reply, Result, State};

handle_call(next, _From, #state{mod=Mod} = State) ->
    Result = Mod:next(),
    {reply, Result, State};

handle_call({next, Id}, _From, #state{mod=Mod} = State) ->
    Result = Mod:next(Id),
    {reply, Result, State};

handle_call(restore_all, _From, #state{mod=Mod} = State) ->
    Result = Mod:restore_all(),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{?MODULE, unknown_call, Call}]),
    error_logger:info_report([{?MODULE, self()}]),
    {reply, ok, State}.

handle_info(_Info, State) ->
    {nopreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

create_area() ->
    gen_server:call(?MODULE, create_area).

save(Id, State) ->
    gen_server:call(?MODULE, {save, Id, State}).

restore(Id) ->
    gen_server:call(?MODULE, {restore, Id}).

next() ->
    gen_server:call(?MODULE, next).

next(Id) ->
    gen_server:call(?MODULE, {next, Id}).

restore_all() ->
    gen_server:call(?MODULE, restore_all).
