-module(spammer).

%-behaviour(obj).

-include("obj.hrl").

-export([
    start_link/1,
    init/1,
    loop/1,
    send_spam/1,
    rand_second/0
    ]).

start_link(State) ->
    Pid = spawn_link(?MODULE, init, [State]),
    {ok, Pid}.

init(State) ->
    {ok, NewState} = obj:init(State),
    timer:apply_after(rand_second(), ?MODULE, send_spam, [self()]),
    ?MODULE:loop(NewState).

loop(State) ->
    receive 
        {migrate, Area} ->
            obj:migrate(Area, ?MODULE, State, State);
        {node_migrate, Node} ->
            obj:node_migrate(Node, ?MODULE, State, State);
        {move, _Pos} ->
            ?MODULE:loop(State);
        {spam_from, _From} ->
            %io:format("SPAM FROM ~p.~n", [From]),
            %io:format("Updated SPAM from ~p.~n", [From]),
            ?MODULE:loop(State);
        send_spam ->
            std_funs:area_event({spam_from, self()}),
            timer:apply_after(rand_second(), ?MODULE, send_spam, [self()]),
            ?MODULE:loop(State);
        die ->
            ok;
        Event ->
            error_logger:info_report([{event, Event}]),
            ?MODULE:loop(State)
    after 1000 ->
        ?MODULE:loop(State)
    end.

rand_second() ->
    random:uniform(1000) + 500.

send_spam(Pid) ->
    Pid ! send_spam.

