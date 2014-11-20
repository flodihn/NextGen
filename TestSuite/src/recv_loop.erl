-module(recv_loop).

-export([
    recv_loop/3
    ]).

-include("state.hrl").
-include("protocol.hrl").

-define(TIMEOUT, 200).

recv_loop(Owner, Socket, #recv_state{id=Id, bytes_recv=BytesRecv,
    	cmds_recv=CmdsRecv, resp_times=RespTimes, 
		debug_output=DebugOutput} = State) ->
    % Since we don't need flow controler in recv loop whe call it before
    % data is received.
    inet:setopts(Socket, [{active, once}]),
    receive 
        {get_state} ->
            Owner ! State;
        {tcp, Socket, <<?NOTIFY_PONG, BinTime/binary>> = Data} ->
			Diff = binary_time_to_diff(BinTime),
            %io:format("Got ping ms: ~p.~n", [Diff/1000]),
            NewBytesRecv = BytesRecv + byte_size(Data) + 2,
			recv_loop(Owner, Socket, 
                State#recv_state{bytes_recv=NewBytesRecv, 
                cmds_recv=CmdsRecv + 1, resp_times=[Diff] ++ RespTimes});
        {tcp, Socket, <<?OBJ_DIR, IdLen/integer, Id:IdLen/binary, 
            	_X/little-float, _Y/little-float, Z/little-float, 
            	BinTime/binary>> = Data} ->
			Diff = time(),
			%Diff = binary_time_to_diff(BinTime),
            %io:format("OBJ_DIR ms: ~p.~n", [Diff/1000]),
            NewBytesRecv = BytesRecv + byte_size(Data) + 2,
			recv_loop(Owner, Socket, 
                State#recv_state{bytes_recv=NewBytesRecv, 
                cmds_recv=CmdsRecv + 1, resp_times=[Diff] ++ RespTimes});
        {tcp, Socket, <<?OBJ_SPEED, IdLen/integer, Id:IdLen/binary,
                 Speed/little-float, BinTime/binary>> = Data} ->
			Diff = binary_time_to_diff(BinTime),
            NewBytesRecv = BytesRecv + byte_size(Data) + 2,
            %io:format("OBJ_SPEED ms: ~p.~n", [Diff/1000]),
			recv_loop(Owner, Socket, 
                State#recv_state{bytes_recv=NewBytesRecv, 
                cmds_recv=CmdsRecv + 1, resp_times=[Diff] ++ RespTimes});
        {tcp, Socket, <<?TEST_STATE_UPDATE, BinTime/binary>> = Data} ->
			Diff = binary_time_to_diff(BinTime),
            NewBytesRecv = BytesRecv + byte_size(Data) + 2,
            %io:format("TEST STATE UPDATE MS: ~p.~n", [Diff/1000]),
			recv_loop(Owner, Socket, 
                State#recv_state{bytes_recv=NewBytesRecv, 
                cmds_recv=CmdsRecv + 1, resp_times=[Diff] ++ RespTimes});
        {tcp, Socket, Data} ->
			debug(Data, DebugOutput),
            NewBytesRecv = BytesRecv + byte_size(Data) + 2,
            recv_loop(Owner, Socket, 
                State#recv_state{bytes_recv=NewBytesRecv, 
                cmds_recv=CmdsRecv + 1});
        Other ->
            error_logger:error_report([{unknown_data, Other}])
    after
        ?TIMEOUT ->
            recv_loop(Owner, Socket, State)
    end.
 

binary_time_to_diff(Binary) ->
	case Binary of
		<<>> -> -1;
		_ValidTime ->
            Time = binary_to_term(Binary),
            timer:now_diff(now(), Time) / 1000
	end.

debug(Msg, false) ->
	ok;

debug(Msg, true) ->
	io:format("~p.~n", [Msg]).

