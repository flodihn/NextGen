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
			add_observer(ObserverPid)
		after 100 ->
			timeout
	end,
	?MODULE:loop(mnesia:dirty_first(?MODULE));

loop(Key) ->
	[Row] = mnesia:dirty_read({?MODULE, Key}),
	%io:format("Row: ~p.~n", [Row]),
	?MODULE:loop(mnesia:dirty_next(?MODULE, Key)).

add_observer(Pid) ->
	case get(observers) of
		undefined ->
			put(observers, [Pid]);
		ObserverList ->
			put(observers, [Pid | ObserverList])
	end.
