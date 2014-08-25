%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the standard implementation for the player library 'libplayer'.
%% The module provides functions to create and login players.
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(twofaction_impl).

-include("char.hrl").
-include("vec.hrl").

% API
-export([
    init/0
    ]).

% handlers
-export([
    assign/0,
	free/1
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the faction library.
%% @end
%%----------------------------------------------------------------------
init() ->
    %{ok, AreaNode} = application:get_env(area_node),
    %rpc:call(AreaNode, areasrv, add_handler, 
    %    [login_char, node(), libplayer, event]),
    %areasrv:add_handler(char_login, libchar),
    ets:new(factions, [named_table]),
	ets:insert(factions, {red, 0}),
	ets:insert(factions, {blue, 0}),
    ok.

%%----------------------------------------------------------------------
%% @doc
%% @spec assign(Id) ->{ok, {faction, Faction}}
%% where
%%      State = obj_state(), 
%%      Id = string(),
%%      Pid = pid() 
%% @type obj_state(). An obj_state record.
%% Purpose: Makes a player login.
%% @end
%%----------------------------------------------------------------------
assign() ->
	[{red, RedCount}] = ets:lookup(factions, red),
	[{blue, BlueCount}] = ets:lookup(factions, blue),
	case RedCount > BlueCount of
		true ->
			ets:update_counter(factions, blue, 1),
			{ok, {faction, blue}};
		false ->
			ets:update_counter(factions, red, 1),
			{ok, {faction, red}}
	end.

free({faction, red}) ->
	ets:update_counter(factions, red, -1);

free({faction, blue}) ->
	ets:update_counter(factions, blue, -1).

