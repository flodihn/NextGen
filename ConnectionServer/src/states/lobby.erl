-module(lobby).

% API
-export([
    event/2
    ]).

-include("protocol.hrl").
-include("charinfo.hrl").
-include("conn_state.hrl").

event(<<?GET_CHAR_LIST>>, State) ->
    {ok, CharSrv} = application:get_env(charsrv),
    CharList = rpc:call(CharSrv, charsrv, get_list, [
        State#conn_state.account]),
    send_char_list(CharList, State),
    {noreply, lobby, State};

event(<<?CHAR_LOGIN, IdLen/integer, Id:IdLen/binary>>, State) ->
	{ok, StartArea} = application:get_env(start_area),
    rpc:call(StartArea, libplayer_srv, login, 
        [self(), Id]),
    receive 
        {char_login, {pid, Pid}, {id, Id}} ->
            error_logger:info_report([{char_login_success, Id}]),
            CharInfo = #charinfo{id=Id, pid=Pid},
            NewState = State#conn_state{charinfo=CharInfo},
            IdLen = byte_size(Id),
            {reply,<<?CHAR_LOGIN_SUCCESS, IdLen, Id/binary>> , playing, 
                NewState}
    end;

event(<<?CHAR_LOGIN>>, State) ->
    {ok, DefaultAreaSrv} = application:get_env(start_area),
	error_logger:info_report({char_login, DefaultAreaSrv}),
    Result = rpc:call(DefaultAreaSrv, libplayer_srv, create, [self()]),
    case Result of 
        {char_login, {pid, Pid}, {id, Id}} ->
            error_logger:info_report([{char_login_success, Id}]),
            CharInfo = #charinfo{id=Id, pid=Pid},
            NewState = State#conn_state{charinfo=CharInfo},
            IdLen = byte_size(Id),
            {reply,<<?CHAR_LOGIN_SUCCESS, IdLen, Id/binary>>, playing, 
                NewState};
        Other ->
            error_logger:info_report([{?MODULE, 
                unknown_char_login_result, Other}]),
            {noreply, lobby, State}
    end;

event(Event, State) ->
    error_logger:info_report([{?MODULE, unknown_event, Event}]),
    {noreply, unknown_command, lobby, State}.

send_char_list([], _State) ->
    done;

send_char_list([{Id, Name} | CharList],
        #conn_state{socket=Socket} = State) ->
    % CharId must be a binary.
    IdLen = byte_size(Id),
    NameLen = byte_size(Name),
    connection:socket_send(Socket, <<?NOTIFY_CHAR_AVAILABLE,
        IdLen, Id/binary, NameLen, Name/binary>>),
    send_char_list(CharList, State).


