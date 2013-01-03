-module(libgeo).
-behaviour(gen_server).

% API
-export([
    create_area/0,
    join/1,
    event/1,
    event/2,
    set_pos/1,
    get_pos/0,
    set_size/1,
    get_size/0,
    set_shape/1,
    query_area/1,
    set_neighbour/2,
    del_neighbour/1,
    get_neighbour/1,
    calc_new_pos/2
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

handle_call({event, Event}, _From, #state{mod=Mod} = State) ->
    Result = Mod:Event(),
    error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call({event, Event, Args}, _From, #state{mod=Mod} = State) ->
    Result = Mod:Event(Args),
    error_logger:info_report([{?MODULE, self()}]),
    {reply, Result, State};

handle_call(create_area, _From, #state{mod=Mod} = State) ->
    Result = Mod:create_area(),
    {reply, Result, State};

handle_call({join, Node}, _From, #state{mod=Mod} = State) ->
    Result = Mod:join(Node),
    {reply, Result, State};

handle_call({set_pos, Pos}, _From, #state{mod=Mod} = State) ->
    Result = Mod:set_pos(Pos),
    {reply, Result, State};

handle_call(get_pos, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_pos(),
    {reply, Result, State};

handle_call({set_size, Pos}, _From, #state{mod=Mod} = State) ->
    Result = Mod:set_size(Pos),
    {reply, Result, State};

handle_call(get_size, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_size(),
    {reply, Result, State};

handle_call({set_shape, Shape}, _From, #state{mod=Mod} = State) ->
    Result = Mod:set_shape(Shape),
    {reply, Result, State};

handle_call({query_area, Pos}, _From, #state{mod=Mod} = State) ->
    Result = Mod:query_area(Pos),
    {reply, Result, State};

handle_call({set_neighbour, Slot, Area}, _From, #state{mod=Mod} = State) ->
    Result = Mod:set_neighbour(Slot, Area),
    {reply, Result, State};

handle_call({del_neighbour, Slot}, _From, #state{mod=Mod} = State) ->
    Result = Mod:del_neighbour(Slot),
    {reply, Result, State};

handle_call({get_neighbour, Slot}, _From, #state{mod=Mod} = State) ->
    Result = Mod:get_neighbour(Slot),
    {reply, Result, State};

handle_call({calc_new_pos, Pos, Border}, 
    _From, #state{mod=Mod} = State) ->
    Result = Mod:calc_new_pos(Pos, Border),
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

event(Event) ->
    gen_server:call(?MODULE, {event, Event}).

event(Event, Args) ->
    gen_server:call(?MODULE, {event, Event, Args}).

create_area() ->
    gen_server:call(?MODULE, create_area).

join(Node) ->
    gen_server:call(?MODULE, {join, Node}).

set_pos(Pos) ->
    gen_server:call(?MODULE, {set_pos, Pos}).

get_pos() ->
    gen_server:call(?MODULE, get_pos).

set_size(Size) ->
    gen_server:call(?MODULE, {set_size, Size}).

get_size() ->
    gen_server:call(?MODULE, get_size).

set_shape(Shape) ->
    gen_server:call(?MODULE, {set_shape, Shape}).

query_area(Pos) ->
    gen_server:call(?MODULE, {query_area, Pos}).

set_neighbour(Slot, Area) ->
    gen_server:call(?MODULE, {set_neighbour, Slot, Area}).

del_neighbour(Slot) ->
    gen_server:call(?MODULE, {del_neighbour, Slot}).

get_neighbour(Slot) ->
    gen_server:call(?MODULE, {get_neighbour, Slot}).

calc_new_pos(Pos, Border) ->
    gen_server:call(?MODULE, {calc_new_pos, Pos, Border}).


