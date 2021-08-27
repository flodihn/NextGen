-module(connected).

-include("state.hrl").
-include("protocol.hrl").
-include("charinfo.hrl").

-export([
    event/2
    ]).

-record(conn_state, {account}).

% For now we terminate the gen_fsm, perhaps we should't make it still run
% and the player can reconnected with preseved state?
event({tcp_closed, _Socket}, State) ->
    % cleanup here.
    {noreply, exit, State};

event(<<?SIGNUP,
        EmailLen:8/integer, Email:EmailLen/binary,
        PassLen:8/integer, Pass:PassLen/binary,Extra/binary>>,
        State) ->
    {ok, Accsrv} = application:get_env(accsrv),
    Result = rpc:call(Accsrv, accsrv, create, [Email, Pass]),
    case Result of
        {ok, account_created} ->
            error_logger:info_report([{account_created, Email, Pass}]),
            {reply, <<?ACCOUNT_LOGIN_SUCCESS>>, lobby, 
                State#conn_state{account=Email}};
        Error->
            error_logger:info_report([{account_creation_failed, Email, Error}]),
            {reply, <<?ACCOUNT_LOGIN_FAIL>>, connected, State}
    end;

event(<<?ACCOUNT_LOGIN:8/integer, 
    AccLen:8/integer, Account:AccLen/binary,
    _PassLen, Password/binary>>,
    State) ->
    %error_logger:info_report([{account_login, Account, Password}]),
    {ok, AccSrv} = application:get_env(accsrv),
    Result = rpc:call(AccSrv, accsrv, validate, [Account, Password]),
    case Result of
        {ok, match} ->
            {reply, <<?ACCOUNT_LOGIN_SUCCESS>>, lobby, 
                State#conn_state{account=Account}};
        Other ->
            error_logger:error_report([Other]),
            {reply, <<?ACCOUNT_LOGIN_FAIL>>, connected, State}
    end;

event(Event, State) ->
    error_logger:info_report([{unknown_message, Event}]),
    {noreply, connected, State}.


    
