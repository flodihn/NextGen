-module(client).

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
    report/1,
    do_report/2
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

auto_start() ->
    P = start(),
    connect(P),
    account_login(P),
    char_login(P),
    start_play(P),
    P.

start() ->
    crypto:start(),
    spawn(client_loop, loop, []).


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
    Pid ! {self(), char_login}.

char_login(Pid, CharId) ->
    Pid ! {self(), char_login, CharId}.

start_play(Pid) ->
    Pid ! {self(), start_play},
    receive 
        {ok, start_play} ->
            done
    end.

info(Pid) ->
    Pid ! {info},
    ok.

report(Pid) ->
    Pid ! {self(), report},
    receive 
        State ->
            #report{
                cmds_sent = State#client_state.cmds_sent,
                cmds_recv = State#client_state.cmds_recv,
                bytes_sent = State#client_state.bytes_sent,
                bytes_recv = State#client_state.bytes_recv}
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
            From ! State#client_state{bytes_recv=BytesRecv, 
                cmds_recv=CmdsRecv}
    end.
