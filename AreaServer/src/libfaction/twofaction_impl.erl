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
	free/1,
	get_spawn_point/2
    ]).

-record(state, {red_spawn_points=[], blue_spawn_points=[]}).

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
	State = #state{
		red_spawn_points=[
			#vec{x=1650, y=5.5,z=125},
			#vec{x=1425, y=5.5,z=125},
			#vec{x=1165, y=5.5,z=80},
			#vec{x=625, y=5.5,z=80},
			#vec{x=415, y=5.5,z=85},
			#vec{x=100, y=5.5,z=50},
			#vec{x=890, y=5.5,z=80},
			#vec{x=1875, y=5.5,z=130}
		],
		blue_spawn_points=[
			#vec{x=125, y=5.5,z=1875},
			#vec{x=375, y=5.5,z=1850},
			#vec{x=625, y=5.5,z=1850},
			#vec{x=850, y=5.5,z=1875},
			#vec{x=1125, y=5.5,z=1875},
			#vec{x=1375, y=5.5,z=1875},
			#vec{x=1650, y=5.5,z=1875},
			#vec{x=1375, y=5.5,z=1875}
		]
	},
    {ok, State}.

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

get_spawn_point(red, #state{red_spawn_points=RedSpawnPoints}) ->
	get_random_spawn_point(RedSpawnPoints);

get_spawn_point(blue, #state{blue_spawn_points=BlueSpawnPoints}) ->
	get_random_spawn_point(BlueSpawnPoints).

get_random_spawn_point(List) ->
	Index = random:uniform(length(List)),
	SpawnPoint = lists:nth(Index, List),
	{ok, SpawnPoint}.
