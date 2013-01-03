-module(connsrv).
-behaviour(gen_server).

-define(LISTEN_PROCS, 10).
-define(PORTNR, 2000).
-define(TCP_OPTS, [binary, {nodelay, true}, {packet, 2}, {active, true}, {reuseaddr, true}]).

% External exports
-export([
    start_link/1
    ]).

% Internal exports
-export([
    listen_proc/1,
    spawn_accept/1
    ]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

% API
-export([
    get_connections/0
    ]).

% server state
-record(state, {
    lsocket,
    module,
    connections = 0
    }).

start_link(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Module, []).
    
init(Module) ->
    case gen_tcp:listen(?PORTNR, ?TCP_OPTS) of
        {ok, LSocket} ->
            %spawn_accept(LSocket),
            spawn_listen_pool(LSocket),
            {ok, #state{lsocket = LSocket, module = Module}};
        Reason ->
            {stop, Reason}
    end.

%handle_call({new_connection, Pid, ConnSocket}, _From, 
%    #state{lsocket = LSocket, module=Module} = State) ->
%    Module:dispatch(ConnSocket);

%handle_call({accept}, _From, #state{accept_pid = undefined,
%    listensock = LSocket} = State) ->
%    Pid = spawn_link(fun() -> accept(Server, LSocket) end),
%    {reply, ok, State#state{accept_pid = Pid}};

%handle_call({connect, Pid, _Socket, Server}, _From, #state{accept_pid = Pid,
%    listensock = LSocket, module = Module} = State) ->
%    NewAcceptPid = spawn_link(fun() -> accept(Server, LSocket) end),
%    erlang:unlink(Pid),
%    {reply, {ok, Module}, State#state{accept_pid = NewAcceptPid}};
%
%handle_call({process_request, Request}, _From, #state{module=M} = State) ->
%    Reply = M:process_request(Request),
%    {reply, Reply, State};

handle_call({connect, ClientSocket}, _From, #state{lsocket=_LSocket, 
    module=Module, connections=Connections} = State) ->
    Result = Module:dispatch(ClientSocket),
    %spawn_accept(LSocket),
    {reply, Result, State#state{connections=Connections + 1}};

handle_call(get_connections, _From, 
    #state{connections=Connections} = State) ->
    {reply, Connections, State};

handle_call(Call, _From, State) ->
    error_logger:error_report([{undefined_call, Call}]),
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {nopreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    io:format("terminate/2 ~p ~p.~n", [Reason, State]).

spawn_listen_pool(LSocket) ->
    spawn_listen_pool(LSocket, ?LISTEN_PROCS).

spawn_listen_pool(_LSocket, 0) ->
    done;

spawn_listen_pool(LSocket, Procs) ->
    spawn_link(?MODULE, listen_proc, [LSocket]),
    spawn_listen_pool(LSocket, Procs -1).

listen_proc(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            case gen_server:call(?MODULE, {connect, Socket}) of 
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid),
                    listen_proc(LSocket);
                 Error ->
                    error_logger:info_report([{listen_proc, Error}]),
                    listen_proc(LSocket)
            end;
         Error ->
             error_logger:info_report([{listen_proc, Error}]),
             listen_proc(LSocket)
    end.

spawn_accept(LSocket) ->
    spawn_link(fun() -> accept(LSocket) end).

accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            error_logger:info_report([{"Connection from:", Socket}]),
            case gen_server:call(?MODULE, {connect, Socket}) of
                {ok, _Pid} ->
                    %gen_tcp:controlling_process(Socket, Pid);
                    ok;
                Error ->
                    error_logger:info_report([{"gen_server:call", 
                        connect_error, Error}])
            end;
        NotOK ->
            error_logger:info_report([{"gen_tcp:accept", NotOK}])
    end.

get_connections() ->
    gen_server:call(?MODULE, get_connections).
