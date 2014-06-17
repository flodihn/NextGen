%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is simple implementation module for the environment library 
%% 'libnv.srv'.
%% The module provides functions for getting and setting the area 
%% terrain and skybox.
%% @end
%%----------------------------------------------------------------------
-module(libenv_simple_env).

-include("env_data.hrl").

% API
-export([
    init/0
    ]).

% handlers
-export([
    create_area/0,
    join_area/1,
    get_terrain/0,
    set_terrain/1,
    get_skybox/0,
    set_skybox/1
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the library.
%% @end
%%----------------------------------------------------------------------
init() ->
    time_daemon:init(),
    %areasrv:add_handler(get_skybox, libenv),
    %areasrv:add_handler(get_terrain, libenv).
    ok.

%%----------------------------------------------------------------------
%% @spec create_area() -> ok
%% @doc
%% Called when a new area is created, creates necessary mnesia tables.
%% @end
%%----------------------------------------------------------------------
create_area() ->
    mnesia:create_table(env_data,
        [{disc_copies, [node()]},
        {attributes, record_info(fields, env_data)}]),
    ok.

join_area(Node) ->
    rpc:call(Node, mnesia, add_table_copy,
        [env_data, node(), disc_copies]),
    ok.   

%%----------------------------------------------------------------------
%% @spec set_terrain(Terrain) -> ok
%% where
%%      Terrain = string()
%% @doc
%% Set the area to have a specific terrain.
%% @end
%%----------------------------------------------------------------------
set_terrain(Terrain) ->
    F = fun() ->
        mnesia:write(#env_data{key=terrain, value=Terrain})
    end,
    mnesia:transaction(F),
    ok.

%%----------------------------------------------------------------------
%% @spec set_skybox(Skybox) -> ok
%% where
%%      Module = string()
%% @doc
%% Set the area to have a skybox.
%% @end
%%----------------------------------------------------------------------
set_skybox(Skybox) ->
    F = fun() ->
        mnesia:write(#env_data{key=skybox, value=Skybox})
    end,
    mnesia:transaction(F),
    ok.

%%----------------------------------------------------------------------
%% @spec get_skybox() -> {skybox, Skybox} | {error, no_skybox}
%% where
%%      Skybox = string()
%% @doc
%% Returns the skybox.
%% @end
%%----------------------------------------------------------------------
get_skybox() ->
    case mnesia:dirty_match_object(#env_data{key=skybox, value='$1'}) of
        [Rec] ->
            {skybox, Rec#env_data.value};
        [] ->
            {error, no_skybox}
    end.

%%----------------------------------------------------------------------
%% @spec get_terrain() -> {terrain, Terrain} | {error, no_terrain}
%% where
%%      Terrain = string()
%% @doc
%% Returns the terrain.
%% @end
%%----------------------------------------------------------------------
get_terrain() ->
    case mnesia:dirty_match_object(#env_data{key=terrain, value='$1'}) of
        [Rec] ->
            {terrain, Rec#env_data.value};
        [] ->
            {error, no_terrain}
    end.

