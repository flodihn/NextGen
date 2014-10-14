-module(connected).

-include("state.hrl").
-include("protocol.hrl").
-include("charinfo.hrl").


-export([
    event/2
    ]).

% For now we terminate the gen_fsm, perhaps we should't make it still run
% and the player can reconnected with preseved state?
event({tcp_closed, _Socket}, State) ->
	% cleanup here.
    {noreply, exit, State};       

event(<<?PLAY/integer>>, State) ->
   	{ok, DefaultAreaSrv} = application:get_env(start_area),
    rpc:call(DefaultAreaSrv, libplayer_srv, create, [self()]),
	receive
        {char_login, {pid, Pid}, {id, Id}} ->
            CharInfo = #charinfo{id=Id, pid=Pid},
            NewState = State#state{charinfo=CharInfo},
			IdLen = byte_size(Id),
			connection:socket_send(State#state.socket, 
				<<?CHAR_LOGIN_SUCCESS, IdLen, Id/binary>>),
			{noreply, playing, NewState};
        Error ->
    		error_logger:info_report({?MODULE, error, Error}),
            {noreply, connected, State}
		after 10000 ->
    		error_logger:info_report({?MODULE, error, timeout}),
            {noreply, connected, State}
    end;

event(<<?OBSERVE:8/integer, 24, "IuJq/11/WyIEs32XoSUaCQ==">>, State) ->
	error_logger:info_report({?MODULE, observing}),
   	{ok, DefaultAreaSrv} = application:get_env(start_area),
    rpc:call(DefaultAreaSrv, liblog_srv, add_observer, [self()]),
    {noreply, observing, State};

event(Event, State) ->
    error_logger:info_report([{unknown_message, Event}]),
    {noreply, connected, State}.


    
