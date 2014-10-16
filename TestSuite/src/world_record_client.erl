-module(world_record_client).

% External functions
-export([
    auto_start/0,
    start/0,
    connect/1,
    connect/2,
    connect/3,
    account_login/1,
    account_login/3,
    char_login/1,
    char_login/2,
    start_play/1,
    info/1,
    report/1
    ]).

% Internal exports
-export([
    loop/0
    ]).

% Only exported during deveopment
-export([
    str_to_binary/1
    ]).

-include("state.hrl").
-include("report.hrl").
-include("protocol.hrl").

-record(vec, {x, y, z}).

-define(TCP_OPTS, [binary, {packet, 2}, {active, once}]).

-define(DEFAULT_HOST, "localhost").
%-define(DEFAULT_HOST, "dev.abydosonline.com").
-define(DEFAULT_PORT, 2000).
-define(DEFAULT_ACCOUNT_NAME, "Account").
-define(DEFAULT_ACCOUNT_PASSWORD, "Password").
-define(TIMEOUT, 10000).

-define(CMD_INTERVAL, 500).

auto_start() ->
	crypto:init(),
    P = start(),
    connect(P),
    account_login(P),
    char_login(P),
    start_play(P),
    P.

start() ->
    crypto:start(),
    spawn(?MODULE, loop, []).

connect(Pid) ->
    connect(Pid, ?DEFAULT_HOST, ?DEFAULT_PORT),
    ok.

connect(Pid, Host) ->
    connect(Pid, Host, ?DEFAULT_PORT),
    ok.

connect(Pid, Host, Port) ->
    Pid ! {self(), connect, Host, Port},
    receive 
        {ok, connected} ->
            done
    end.

account_login(Pid) ->
    account_login(Pid, ?DEFAULT_ACCOUNT_NAME, ?DEFAULT_ACCOUNT_PASSWORD).

account_login(Pid, Account, Password) ->
    Pid ! {self(), account_login, Account, Password},
    receive 
        {ok, account_login} ->
            done
    end.

char_login(Pid) ->
    Pid ! {self(), char_login},
    receive 
        {ok, char_login} ->
            done
    end.

char_login(Pid, CharId) ->
    Pid ! {self(), char_login, CharId},
    receive
        {ok, char_login} ->
            done
    end.

start_play(Pid) ->
    Pid ! {self(), start_play},
    receive 
        {ok, start_play} ->
            done
    end.

info(Pid) ->
    Pid ! {info},
    ok.

wait_for_new_pos(Socket, Id) ->
	case recv(Socket) of
        <<?OBJ_POS, IdLen/integer, NewId:IdLen/binary,
			X/little-float, Y/little-float, Z/little-float>> ->
			#vec{x=X, y=Y, z=Z};
		Other ->
			wait_for_new_pos(Socket, Id)
	end.

report(Pid) ->
    Pid ! {self(), report},
    receive 
        State ->
            #report{
                cmds_sent = State#client_state.cmds_sent,
                cmds_recv = State#client_state.cmds_recv,
                bytes_sent = State#client_state.bytes_sent,
                bytes_recv = State#client_state.bytes_recv,
				resp_times = State#client_state.resp_times,
				avg_resp_time = State#client_state.avg_resp_time}
    end.

loop() ->
    receive 
        {From, connect, Host, Port} ->
            {ok, Socket} = gen_tcp:connect(Host, Port, ?TCP_OPTS),
            From ! {ok, connected},
            loop(Socket, undefined, undefined);
        Data ->
            io:format("Unknown command: ~p~n", Data), 
            loop()
    end.

loop(Socket, Id, Pos) ->
    receive 
        {From, char_login} ->   
            send(Socket, <<?PLAY/integer>>),
            case recv(Socket) of
                <<?CHAR_LOGIN_SUCCESS, IdLen/integer, 
                    NewId:IdLen/binary>> ->
                    From ! {ok, char_login},
					% Receive initial position
					StartPos = wait_for_new_pos(Socket, NewId),
                    loop(Socket, NewId, StartPos);
                <<?CHAR_LOGIN_FAIL>> ->
                    io:format("Char login fail~n");
                Data ->
                    io:format("Char login unknown data: ~p.~n", [Data])
            end;
        {From, start_play} ->
            RecvPid = spawn_link(recv_loop, recv_loop, [
				self(), Socket, #recv_state{id=Id}]),
            gen_tcp:controlling_process(Socket, RecvPid),
            From ! {ok, start_play},
            play_loop(Socket, #client_state{recv_proc=RecvPid, pos=Pos,
				time_since_last_cmd=now()});
        Data ->
            % While not all clients are has been logged in we might 
            % receive some valid
            % data, but since we have not started playing yet we just 
            % ignore it.
            io:format("Unknown data received: ~p~n", [Data]), 
            loop(Socket, Id, Pos)
    end.

play_loop(Socket, 
    #client_state{cmds_sent=CmdsSent, bytes_sent=BytesSent, 
		pos=Pos} = State) ->
    receive
        {From, report} ->
            do_report(From, State);
        {info} ->
            io:format("~p~n", [State]),
            play_loop(Socket, State);
        Data ->
            io:format("Unknown data received: ~p~n", [Data]), 
            play_loop(Socket, State)
    after ?CMD_INTERVAL ->
            {Cmd, NewClientState} = get_rand_cmd(State),
            Sent = send(Socket, Cmd),
            NewBytesSent = BytesSent + Sent,
            NewCmdsSent = CmdsSent + 1,
            %io:format("Bytes sent: ~p.~n", [NewBytesSent]),
            play_loop(Socket, NewClientState#client_state{
                cmds_sent=NewCmdsSent, bytes_sent=NewBytesSent,
				time_since_last_cmd=now()})
            %play_loop(Socket, State#client_state{
            %    cmds_sent=1, bytes_sent=1})
    end.

% Function that gathers the amount of bytes recevied from the recv_loop 
% process.
do_report(From, 
    #client_state{recv_proc=RecvPid} = State) ->
    RecvPid ! {get_state},
    receive 
        RecvState ->
            CmdsRecv = RecvState#recv_state.cmds_recv,
            BytesRecv = RecvState#recv_state.bytes_recv,
			RespTimes = RecvState#recv_state.resp_times,
            From ! State#client_state{bytes_recv=BytesRecv, 
                cmds_recv=CmdsRecv, resp_times=RespTimes}
    end.

get_rand_cmd(#client_state{pos=Pos, 
		time_since_last_cmd=TimeSinceLastCmd, last_vel=LastVel,
		last_dir=LastDir} = State) ->
	DeltaTime = timer:now_diff(now(), TimeSinceLastCmd) / 1000000,
	{VelX, VelY, VelZ} = maybe_change_vector(LastVel, 5),
	{DirX, DirY, DirZ} = maybe_change_vector(LastDir, 1),
	PosX = Pos#vec.x + (VelX * DeltaTime),
	PosY = Pos#vec.y + (VelY * DeltaTime),
	PosZ = Pos#vec.z + (VelZ * DeltaTime),
	%error_logger:info_report({sending, PosX, PosY, PosZ}),
    {<<?SYNC_POS,
		PosX/little-float, PosY/little-float, PosZ/little-float,
		DirX/little-float, DirY/little-float, DirZ/little-float,
		VelX/little-float, VelY/little-float, VelZ/little-float>>,
		State#client_state{pos=#vec{x=PosX, y=PosY, z=PosZ},
			last_vel={VelX, VelY, VelZ}, last_dir={DirX, DirY, DirZ}}}.

maybe_change_vector(LastVec, Multiplier) ->
	case LastVec of
		undefined ->
	  		{rand_float() * Multiplier, 0 * Multiplier, 
				rand_float() * Multiplier};
		_RealVec ->
			case crypto:rand_uniform(0, 50) of
				1 ->
	  				{rand_float() * Multiplier, 0 * Multiplier, 
						rand_float() * Multiplier};
				_Other ->
					LastVec
			end
	end.
		
				

rand_float() ->
    F = crypto:rand_uniform(0, 10) * 0.1,
    case crypto:rand_uniform(0, 2) of 
        0 ->
            F;
        1 ->
            -F
    end.       

send(Socket, Data) when is_binary(Data) ->
    % Byte size of the data plus 2 byte header. Ignoring the TCP/IP header
    % overhead.
    Sent = byte_size(Data) + 2,
    %io:format("Sending header: 2 payload: ~p data: ~p.~n", [Sent, Data]),
    gen_tcp:send(Socket, Data),
    Sent.

recv(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            Data;
        Other ->
            error_logger:info_report([{recv, Other}])
    after 10000 ->
        error_logger:error_report({"No response from server"})
    end.

str_to_binary(Str) ->
    Len = length(Str),
    Bin = list_to_binary(Str),
    <<Len/integer, Bin/binary>>.

