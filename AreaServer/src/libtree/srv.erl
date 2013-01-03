%%---------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% The server for the tree algoritm library 'libtree'.
%% @end
%%---------------------------------------------------------------------
-module(libtree.srv).
-behaviour(gen_server).

-import(gen_server).
-import(error_logger).

% API
-export([
    create_area/0,
    join_area/1,
    increase/0,
    decrease/0,
    info/0,
    assign/4,
    event/3,
    event/4,
    set_size/1,
    get_size/0,
    handle_exit/2
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

-record(state, {mod, tree_state}).

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
    %process_flag(trap_exit, true),
    {ok, TreeState} = Module:init(),
    {ok, #state{mod=Module, tree_state=TreeState}}.

%% @doc
%% @private

handle_call(create_area, _From, 
    #state{mod=Mod, tree_state=TreeState} = State) ->
    {ok, NewTreeState} = Mod:create_area(TreeState),
    {noreply, State#state{tree_state=NewTreeState}};

handle_call({join_area, Node}, _From, #state{mod=Mod} = State) ->
    Mod:join_area(Node),
    {noreply, State};

handle_call(increase, _From, #state{mod=Mod, tree_state=TreeState} = 
    State) ->
    {Result, NewTreeState} = Mod:increase(TreeState),
    {reply, Result, State#state{tree_state=NewTreeState}};

handle_call(decrease, _From, #state{mod=Mod, tree_state=TreeState} = 
    State) ->
    {Result, NewTreeState} = Mod:decrease(TreeState),
    {reply, Result, State#state{tree_state=NewTreeState}};

handle_call({assign, {id, Id}, {pid, Pid}, {pos, Pos}, 
    {current_quad, Quad}}, _From, 
    #state{mod=Mod, tree_state=TreeState} = State) ->
    link(Pid),
    Result = Mod:assign(Id, Pid, Pos, Quad, TreeState),
    {reply, Result, State};

handle_call(get_size, _From, 
    #state{mod=Mod, tree_state=TreeState} = State) ->
    Result = Mod:get_size(TreeState),
    {reply, Result, State};

handle_call(Call, _From, State) ->
    error_logger:info_report([{unknown_call, Call, State}]),
    {reply, ok, State}.
%% @end

%% @private
handle_info(_Info, State) ->
    {nopreply, State}.

%% @private

handle_cast(info, #state{mod=Mod, tree_state=TreeState} = State) ->
    Mod:info(TreeState),
    {noreply, State};

handle_cast({set_size, Size}, #state{mod=Mod} = State) ->
    {ok, Result, NewTreeState} = Mod:set_size(Size),
    {reply, Result, State#state{tree_state=NewTreeState}};

handle_cast({event, {from, From}, {quad, Quad}, {'fun', Fun}, 
    {args, Args}}, #state{mod=Mod, tree_state=TreeState} = State) ->
    Mod:event(From, Quad, Fun, Args, TreeState),
    {noreply, State};

handle_cast({handle_exit, Id, Quad}, #state{mod=Mod} = State) ->
    Mod:handle_exit(Id, Quad),
    {noreply, State};


%% @private
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{mod=_Mod}) ->
    %error_logger:info_report([{libchar, terminating, cleaning_up}]),
    %Mod:unregister_events().
    ok.

create_area() ->
    gen_server:call(?MODULE, create_area).

join_area(Node) ->
    gen_server:call(?MODULE, {join_area, Node}).

increase() ->
    gen_server:call(?MODULE, increase).

decrease() ->
    gen_server:call(?MODULE, decrease).

info() ->
    gen_server:cast(?MODULE, info).

assign(Id, Pid, Pos, Quad) ->
    gen_server:call(?MODULE, {assign, {id, Id}, {pid, Pid}, {pos, Pos},
        {current_quad, Quad}}).

event(From, Quad, Fun) when is_pid(From)->
    event(From, Quad, Fun, []).

event(From, Quad, Fun, Args) when is_pid(From)->
    gen_server:cast(?MODULE, {event, {from, From}, {quad, Quad}, 
        {'fun', Fun}, {args, Args}}).

set_size(Size) ->
    gen_server:cast(?MODULE, {set_size, Size}).

get_size() ->
    gen_server:call(?MODULE, get_size).
    
handle_exit(Id, Quad) ->
    gen_server:cast(?MODULE, {handle_exit, Id, Quad}).
    
