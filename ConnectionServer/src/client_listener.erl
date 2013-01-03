-module(client_listener).

-include("protocol.hrl").

-export([
    start/2
    ]).

% Internal exports
-export([
    %recv_header/2
    wait_for_data/2
    ]).

start(Receiver, Socket) ->
    error_logger:info_report([{starting_receiver_process}]),
    %connection:socket_send(Socket, ?CONN_ESTABLISHED),
    spawn_link(?MODULE, wait_for_data, [Receiver, Socket]).

wait_for_data(Receiver, Socket) ->
    error_logger:info_report([{wait_for_data, Socket, Receiver}]),
    case gen_tcp:recv(Socket, 2, 2000) of
        {ok, <<HeaderSize:16/integer>>} ->
            error_logger:info_report([{got_header_size, HeaderSize}]),
            case gen_tcp:recv(Socket, HeaderSize) of 
                {ok, Data} ->
                    error_logger:info_report([{got_data, Data}]),
                    Receiver ! {client_data, Data},
                    wait_for_data(Receiver, Socket);
                DataError ->
                    error_logger:error_report([tcp_data_error, DataError])
            end;
        HeaderError ->
            error_logger:error_report([tcp_header_error, HeaderError,
                Receiver, Socket])
    end.
        
        %error_logger:info_report([{received_data, Data}]),
        %    Receiver ! {client_data, Data},
        %    wait_for_data(Receiver, Socket)

%recv_header(Receiver, Socket) ->
    %case gen_tcp:recv(Socket, 1) of
    %    {error, closed} ->
    %        error_logger:info_report([{?MODULE, socket_closed}]),
    %        exit(normal);
    %    {ok, <<HeaderSize:8/binary>>} ->
    %        error_logger:info_report([{received_binary, HeaderSize}]),
    %        recv_msg(Receiver, Socket, HeaderSize),
    %        recv_header(Receiver, Socket);P
    %    {ok, Binary} ->
    %        error_logger:info_report([{received_unknown_binary, Binary}]),
    %        recv_header(Receiver, Socket)
    %end.

%recv_msg(Receiver, Socket, Size) ->
%    case gen_tcp:recv(Socket, Size) of
%        {ok, Msg} ->
%            error_logger:info_report([{client_listener, msg, Msg}]),
%            Receiver ! {tcp_data, decode_msg(Msg)};
%        {error, Error} ->
%            error_logger:info_report([{client_listener, recv_header,
%                Error}])
%    end.

%decode_event(<<AccLen:8/integer, Account:AccLen/binary,
%    PassLen:8/integer, Password:PassLen/binary>>) ->
%    error_logger:info_report([{AccLen, binary_to_list(Account),
%        PassLen, binary_to_list(Password)}]);

%decode_event(<<Event:8/integer, Rest/binary>>) ->
    
%decode_msg(Binary) ->
%    error_logger:info_report([{error, no_binary_match, Binary}]).

