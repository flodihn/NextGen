%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(observer_logger).

-include("char.hrl").
-include("vec.hrl").

% API
-export([
    init/0
    ]).

% handlers
-export([
    log/1,
	view_log/1,
	clear_log/2,
	create_area/0,
	get_loop_procs/0,
	notify_observers/3,
	add_test_entries/0,
	spawn_loop_procs/2,
	add_observer_to_loop_procs/2,
	remove_observer_from_loop_procs/2
    ]).

-record(obpos_log, {id, data, time}).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the logger.
%% @end
%%----------------------------------------------------------------------
init() ->
    {ok, []}.

create_area() ->
	mnesia:start(),
    mnesia:create_table(obpos_log,
    	[{ram_copies, [node()]},
    	{attributes, record_info(fields, obpos_log)}]),
    ok.

get_loop_procs() ->
	[obpos_log].	

%%----------------------------------------------------------------------
%% @doc
%% @spec log(Data) -> ok
%% where
%%      Data = any() 
%% @end
%%----------------------------------------------------------------------
log({sync_pos, {id, Id}, {log, Data}}) ->
    mnesia:dirty_write(#obpos_log{id=Id, data=Data, time=erlang:now()});

log(Unknown) ->
	error_logger:info_report({ignoring_log_request, Unknown}).


view_log(LogType) ->
	traverse_table_and_show(LogType).

traverse_table_and_show(Table_name)->
    Iterator =  fun(Rec,_)->
    	io:format("~p~n",[Rec]),
		[]
    end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(
				transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

add_test_entries() ->
	RandVec = #vec{
		x=random:uniform(100),
		y=random:uniform(100),
		z=random:uniform(100)},
	log({sync_pos,
		{id, <<"test@foobar#123456789">>}, {log, RandVec}}).

spawn_loop_procs([], Acc) ->
	Acc;

spawn_loop_procs([Module | Rest], Acc) ->
	Pid = Module:init(),
	spawn_loop_procs(Rest, [Pid | Acc]).

add_observer_to_loop_procs([], _ObserverPid) ->
	done;

add_observer_to_loop_procs([Proc | LoopProcs], ObserverPid) ->
	Proc ! {add_observer, {pid, ObserverPid}},
	add_observer_to_loop_procs(LoopProcs, ObserverPid).

remove_observer_from_loop_procs([], _ObserverPid) ->
	done;

remove_observer_from_loop_procs([Proc | LoopProcs], ObserverPid) ->
	Proc ! {remove_observer, {pid, ObserverPid}},
	remove_observer_from_loop_procs(LoopProcs, ObserverPid).

clear_log([], _Id) ->
	done;

clear_log([Proc | LoopProcs], Id) ->
	Proc ! {clear_log, {id, Id}},
	clear_log(LoopProcs, Id).

notify_observers(_Type, _Arg, []) ->
	done;

notify_observers(Type, Arg, [Proc | LoopProcs]) ->
	Proc ! {notification, {Type, Arg}},
	notify_observers(Type, Arg, LoopProcs).
