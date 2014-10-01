%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This module loops over a mnesia table with the same name as the
%% module.
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(obpos_log).

-include("vec.hrl").

% API
-export([
    init/0,
	loop/0,
	loop/1]).

init() ->
	spawn_link(?MODULE, loop, []).

loop() ->
	?MODULE:loop(mnesia:dirty_first(?MODULE)).

loop('$end_of_table') ->
	receive 
		{add_observer, {pid, ObserverPid}} ->
			add_observer(ObserverPid);
		{remove_observer, {pid, ObserverPid}} ->
			remove_observer(ObserverPid);
		{clear_log, {id, Id}} ->
			clear_log(Id);
		{notification, {deleted, Id}} ->
			send_delete_to_observers(Id)
		after 1000 ->
			timeout
	end,
	?MODULE:loop(mnesia:dirty_first(?MODULE));

loop(Key) ->
	[Row] = mnesia:dirty_read({?MODULE, Key}),
	%error_logger:info_report({dirty_read, Row}),
	Observers = get(observers),
	case Observers of 
		undefined ->
			%error_logger:info_report({warning, no_observers});
			pass;
		_List ->
			notify_observer(Observers, Row)
	end,
	?MODULE:loop(mnesia:dirty_next(?MODULE, Key)).

add_observer(Pid) ->
	%error_logger:info_report({add_observer, Pid}),
	case get(observers) of
		undefined ->
			put(observers, [Pid]);
		ObserverList ->
			put(observers, [Pid | ObserverList])
	end.

remove_observer(Pid) ->
	%error_logger:info_report({add_observer, Pid}),
	case get(observers) of
		undefined ->
			pass;
		ObserverList ->
			put(observers, lists:delete(Pid, ObserverList))
	end.

notify_observer([], _Row) ->
	ok;

notify_observer([Observer | Rest], Row) ->
	%error_logger:info_report({sending_to_observer, Observer, Row}),
	Observer ! Row, 
	notify_observer(Rest, Row).

clear_log(Id) ->
	F = fun() ->
		mnesia:delete({obpos_log, Id})
	end,
	mnesia:transaction(F).

send_delete_to_observers(Id) ->
		case get(observers) of
		undefined ->
			pass;
		ObserverList ->
			send_delete_to_observers(ObserverList, Id)
	end.

send_delete_to_observers([], _Id) ->
	ok;

send_delete_to_observers([Observer | Rest], Id) ->
	Observer ! {deleted, Id}, 
	notify_observer(Rest, Id).
