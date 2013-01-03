-module(libtree.trunk).

-import(pg2).

-export([
    loop_init/1
    ]).

-export([
    start/2,
    stop/1,
    send_msg/2,
    become_leaf/1
    ]).

-record(state, {group, child_group}).

start(Group, ChildGroup) ->
    spawn_link(?MODULE, loop_init, [#state{group=Group,
        child_group=ChildGroup}]).

stop(Pid) ->
    Pid ! stop.

loop_init(#state{group=Group} = State) ->
    pg2:join(Group, self()),
    loop(State).

loop(#state{group=Group, child_group=ChildGroup} = State) ->
    receive 
        stop ->
            pg2:leave(Group),
            stop;
        become_leaf ->
            libtree.leaf:loop({state, Group});
        {entry_msg, Msg} ->
            List = pg2:get_members(Group),
            send_group_msg(List, Msg),
            loop(State);
        Msg ->
            List = pg2:get_local_members(ChildGroup),
            send_group_msg(List, Msg),
            loop(State)
    end.

send_group_msg([Pid | Rest], Msg) ->
    Pid ! Msg,
    send_group_msg(Rest, Msg);

send_group_msg([], _Msg) ->
    done.

send_msg(Pid, Msg) ->
    Pid ! {entry_msg, Msg}.

become_leaf(Pid) ->
    Pid ! become_leaf.

