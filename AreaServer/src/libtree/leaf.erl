-module(libtree.leaf).

-extends(libtree.trunk).

-export([
    start/1,
    loop/1
    ]).

-record(state, {group}).

start(Group) ->
    spawn_link(?MODULE, loop_init, [#state{group=Group}]).

loop(#state{group=Group}) ->
    receive 
        stop ->
            pg2:leave(Group),
            stop;
        Msg ->
            List = pg2:get_local_members(Group),
            libtree.trunk:send_group_msg(List, Msg),
            loop(Group)
    end.

