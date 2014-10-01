-module(observing).

-include("observer_protocol.hrl").
-include("state.hrl").
-include("vec.hrl").

% States
-export([
    event/2
    ]).

event(tcp_closed, State) ->
   	{ok, DefaultAreaSrv} = application:get_env(start_area),
    rpc:call(DefaultAreaSrv, liblog_srv, remove_observer, [self()]),
    {noreply, exit, State};

event({obpos_log, Id, {vec, X, Y, Z}, Dir}, State) ->
	IdStr = make_str(Id),
	{reply, <<?OBSERVER_OBJ_POS, IdStr/binary,
		X/little-float, Y/little-float, Z/little-float>>, observing, State};

event({deleted, Id}, State) ->
	IdStr = make_str(Id),
	{reply, <<?OBSERVER_OBJ_DELETED, IdStr/binary>>, observing, State};

event(Event, State) ->
    error_logger:info_report([{unknown_event, Event}]),
    {noreply, observing, State}.

make_str(Bin) ->
	BinLen = byte_size(Bin),
	<<BinLen:8, Bin/binary>>.		

validate_id(<<_IdLen:8, Id/binary>>, 
		#state{validate_id_regexp=RegExp} = State) ->
	case re:run(Id, RegExp) of
		nomatch ->
			error_logger:info_report({invalid_id, Id}),
			{false, State};
		{match, _} ->
			{true, State}
	end.
