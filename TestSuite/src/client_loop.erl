-module(client_loop).

% External functions
-export([
    loop/0
    ]).

-include("state.hrl").
-include("report.hrl").
-include("protocol.hrl").

-define(TCP_OPTS, [binary, {packet, 2}, {active, once}]).

-define(DEFAULT_HOST, "localhost").
%-define(DEFAULT_HOST, "dev.abydosonline.com").
-define(DEFAULT_PORT, 2000).
-define(DEFAULT_ACCOUNT_NAME, "Account").
-define(DEFAULT_ACCOUNT_PASSWORD, "Password").
-define(TIMEOUT, 10000).

-define(CMD_INTERVAL, 1000).

loop() ->
    receive 
        {From, connect, Host, Port} ->
            {ok, Socket} = gen_tcp:connect(Host, Port, ?TCP_OPTS),
            From ! {ok, connected},
            loop(Socket, undefined);
        Data ->
            io:format("Unknown command: ~p~n", Data), 
            loop()
    end.



loop(Socket, Id) ->
    receive 
        {From, account_login, Account, Pass} ->
            AccountBin = str_to_binary(Account),
            PassBin = str_to_binary(Pass),
            %io:format("Account login...~n", []),
            send(Socket, <<?ACCOUNT_LOGIN/integer, AccountBin/binary, 
                PassBin/binary>>),
            case recv(Socket) of
                <<?CHAR_LOGIN_SUCCESS, IdLen/integer, 
                    NewId:IdLen/binary>> ->
                    From ! {ok, account_login},
                    From ! {ok, char_login},
                    loop(Socket, NewId);
                %<<?ACCOUNT_LOGIN_SUCCESS>> ->
                %    %io:format("Account login success~n"),
                %    From ! {ok, account_login};
                %<<?ACCOUNT_LOGIN_FAIL>> ->
                %    io:format("Account login fail~n");
                Data ->
                    io:format("Account login unknown data: ~p.~n", [Data])
            end,
            loop(Socket, Id);
        {From, char_login} ->   
            loop(Socket, Id);
        %    send(Socket, <<?CHAR_LOGIN/integer>>),
        %    case recv(Socket) of
        %        <<?CHAR_LOGIN_SUCCESS, IdLen/integer, 
        %            NewId:IdLen/binary>> ->
        %            From ! {ok, char_login},
        %            loop(Socket, NewId);
        %        <<?CHAR_LOGIN_FAIL>> ->
        %            io:format("Char login fail~n");
        %        Data ->
        %            io:format("Char login unknown data: ~p.~n", [Data])
        %    end;
        {From, start_play} ->
            RecvPid = spawn_link(recv_loop, recv_loop, [self(), Socket, 
                #recv_state{id=Id}]),
            gen_tcp:controlling_process(Socket, RecvPid),
            From ! {ok, start_play},
            play_loop(Socket, #client_state{recv_proc=RecvPid});
        Data ->
            % While not all clients are has been logged in we might 
            % receive some valid
            % data, but since we have not started playing yet we just 
            % ignore it.
            io:format("Unknown data received: ~p~n", [Data]), 
            loop(Socket, Id)
    end.

play_loop(Socket, 
    #client_state{cmds_sent=CmdsSent, bytes_sent=BytesSent} = State) ->
    receive
        {From, report} ->
            client:do_report(From, State);
        {info} ->
            io:format("~p~n", [State]),
            play_loop(Socket, State);
        Data ->
            io:format("Unknown data received: ~p~n", [Data]), 
            play_loop(Socket, State)
    after ?CMD_INTERVAL ->
            Cmd = get_rand_cmd(),
            Sent = send(Socket, Cmd),
            NewBytesSent = BytesSent + Sent,
            NewCmdsSent = CmdsSent + 1,
            %io:format("Bytes sent: ~p.~n", [NewBytesSent]),
            play_loop(Socket, State#client_state{
                cmds_sent=NewCmdsSent, bytes_sent=NewBytesSent})
    end.

get_rand_cmd() ->
    TimeStamp = term_to_binary(now()),
    case crypto:rand_uniform(1, 4) of
        1 ->
            <<?INCREASE_SPEED, TimeStamp/binary>>;
        2 ->
            <<?DECREASE_SPEED, TimeStamp/binary>>;
        3 ->
            X = rand_float(),
            Y = 0,
            Z = rand_float(), 
            <<?SET_DIR, X/little-float, Y/little-float, Z/little-float,
                TimeStamp/binary>>;
        4 ->
            <<?PING, TimeStamp/binary>>
    end.

rand_float() ->
    F = random:uniform(),
    case random:uniform(2) of 
        1 ->
            F;
        2 ->
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
            error_logger:info_report([{recv, Other}]),
            recv(Socket)
	after 10000 ->
		error_logger:error_report({"No response from server"})
    end.

str_to_binary(Str) ->
    Len = length(Str),
    Bin = list_to_binary(Str),
    <<Len/integer, Bin/binary>>.

