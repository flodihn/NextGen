-module(connection).

-behaviour(gen_fsm).

-include("charinfo.hrl").
-include("state.hrl").
-include("char.hrl").

% States
-export([
    conn_lost/2,
    connected/2,
    lobby/2,
    playing/2
    ]).

% API
-export([
    conn_reply/2,
    socket_send/2
    ]).

% gen_fsm callbacks
-export([
    start_link/1,
    init/1,
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    terminate/3,
    code_change/4
    ]).

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

init(Socket) ->
    %error_logger:info_report([{new_connection, {socket, Socket}}]),
    %client_listener:start(self(), Socket),
    State = #state{socket=Socket},
    {ok, connected, State}.

handle_event(Event, StateName, StateData) ->
    error_logger:info_report([{event, Event, StateName}]),
    {next_state, StateName, StateData}.

% For now we terminate the gen_fsm, perhaps we should't make it still run
% and the player can reconnected with preseved state?
handle_info({tcp_closed, _Socket}, StateName, StateData) ->
    apply(StateName, event, [tcp_closed, StateData]),
    {stop, normal, StateData}; 

handle_info({tcp, Socket, Data}, StateName, 
    	#state{socket=Socket} = State) ->
    %error_logger:info_report([{tcp, Socket, Data}]),
    case apply(StateName, event, [Data, State]) of
        {reply, Reply, NextState, NewState} ->
            socket_send(Socket, Reply),
            {next_state, NextState, NewState};
        {noreply, NextState, NewState} ->
            {next_state, NextState, NewState};
        Error ->
            error_logger:error_report([Error]),
            {next_state, StateName, State}
    end;

handle_info({set_char_pid, Pid}, playing, 
		#state{charinfo=CharInfo} = StateData) ->
	NewCharInfo = CharInfo#charinfo{pid=Pid},
	{next_state, playing, StateData#state{charinfo=NewCharInfo}};

handle_info(Info, playing, #state{socket=Socket} = State) ->
    case apply(playing, event, [Info, State]) of
        {reply, Reply, StateName, NewState} ->
            socket_send(Socket, Reply),
            {next_state, StateName, NewState};
        {noreply, StateName, NewState} ->
            {next_state, StateName, NewState}
    end;

handle_info(_Info, StateName, StateData) ->
    %error_logger:info_report([Info]),
    {next_state, StateName, StateData}.
 
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, test, StateName, StateData}.

code_change(_OldVsn, _StateName, _StateData, _Extra) ->
    ok.

terminate(_Reason, _StateName, _StateData) ->
    ok.

conn_reply(Socket, Reply) ->
    %error_logger:info_report([{debug, conn_reply, Reply}]),
    gen_tcp:send(Socket, term_to_binary(Reply)).

socket_send(Socket, Msg) ->
    %error_logger:info_report([{socket_send, Msg, byte_size(Msg)}]),
    gen_tcp:send(Socket, Msg).

connected(Event, State) ->
    connected:event(Event, State).

lobby(Event, State) ->
    lobby:event(Event, State).

playing(Event, State) ->
    playing:event(Event, State).

conn_lost(Event, State) ->
    conn_lost:event(Event, State).
   
