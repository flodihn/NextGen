-module(libtree.flat_quad).

-import(pg2).
-import(error_logger).

-export([
    loop_init/1
    ]).

-export([
    start/1,
    stop/1,
    send_msg/2
    ]).

-record(state, {group}).

start(Group) ->
    Pid = spawn_link(?MODULE, loop_init, [#state{group=Group}]),
    {ok, Pid}.

stop(Pid) ->
    Pid ! stop.

loop_init(#state{group=Group} = State) ->
    pg2:join(Group, self()),
    loop(State).

loop(#state{group=Group} = State) ->
    receive 
        stop ->
            pg2:leave(Group),
            stop;
        Msg ->
            error_logger:info_report([{group_msg, Msg}]),
            List = pg2:get_members(Group),
            send_group_msg(List, Msg),
            loop(State)
    end.

send_group_msg([Pid | Rest], Msg) when Pid == self() ->
    send_group_msg(Rest, Msg);

send_group_msg([Pid | Rest], Msg) ->
    Pid ! Msg,
    send_group_msg(Rest, Msg);

send_group_msg([], _Msg) ->
    done.

send_msg(Pid, Msg) ->
    Pid ! {entry_msg, Msg}.


