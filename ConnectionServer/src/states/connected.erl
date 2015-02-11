-module(connected).

-include("protocol.hrl").
-include("char.hrl").

-record(state, {socket, account, charinfo}).

-export([
    event/2
    ]).

% For now we terminate the gen_fsm, perhaps we should't make it still run
% and the player can reconnected with preseved state?
event({tcp_closed, _Socket}, StateData) ->
    {stop, normal, StateData};       

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
                State#state{account=Account}};
        Other ->
            error_logger:error_report([Other]),
            {reply, <<?ACCOUNT_LOGIN_FAIL>>, connected, State}
    end;

event(<<?SIGNUP, 
    AccLen:8/integer, Account:AccLen/binary, 
    EmailLen:8/integer, Email:EmailLen/binary,
    _PassLen:8/integer, Pass/binary>>,
    State) ->
    {ok, Accsrv} = application:get_env(accsrv),
    Result = rpc:call(Accsrv, accsrv, create, [Account, Email, 
        Pass]),
    case Result of
        {ok, account_created} ->
            error_logger:info_report([{account_created, Account, Email,
                Pass}]),
            {reply, <<?ACCOUNT_LOGIN_SUCCESS>>, lobby, 
                State#state{account=Account}};
        Error->
            error_logger:info_report([{account_creation_failed, Account, 
                Error}]),
            {reply, <<?ACCOUNT_LOGIN_FAIL>>, connected, State}
    end;

%event({lookup_account, Account}, State) ->
%    {ok, Accsrv} = application:get_env(accsrv),
%    Result = rpc:call(Accsrv, accsrv, lookup, [Account]),
%    {reply, Result, connected, State};

event(Event, State) ->
    error_logger:info_report([{unknown_message, Event}]),
    {next_state, connected, State}.


    
