-module(playing).

-include("conn_state.hrl").
-include("charinfo.hrl").

% States
-export([
    event/2
    ]).

event({client_reply, Data}, State) ->
    error_logger:info_report({client_reply, Data}),
    {reply, Data, playing, State};

event(tcp_closed, State) ->
    {stop, normal, State};

% All data received on the socket is sent to the player for 
% processing.
event(BinarySocketData, State) ->
    game_obj_send(State, BinarySocketData),
    {noreply, playing, State}.

game_obj_send(
    #conn_state{charinfo=#charinfo{pid=P} = ConnState}, 
    BinarySocketData) ->
    P ! {client_request, BinarySocketData, ConnState}.

validate_id(<<Bin/binary>>, #state{validate_id_regexp=undefined} = State) ->
	{ok, RegExp} = re:compile("^[a-zA-Z0-9-_]+@[a-zA-Z0-9_-]+#[0-9]+"),
	validate_id(Bin, State#state{validate_id_regexp=RegExp});

validate_id(
    <<_IdLen:8, Id/binary>>, #state{validate_id_regexp=RegExp} = State) ->
	error_logger:info_report({invalid_id, Id, RegExp}),
	case re:run(Id, RegExp) of
		nomatch ->
			{false, State};
		{match, _} ->
			{true, State}
	end.
