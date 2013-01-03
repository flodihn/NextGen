%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the standard implementation for the player library 'libplayer'.
%% The module provides functions to create and login players.
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(libgod.std_impl).

-import(application).
-import(error_logger).
-import(rpc).
-import(io).

-import(obj_sup).
-import(libstd).
-import(util).
-import(obj).

-include("char.hrl").
-include("vec.hrl").

% API
-export([
    init/0
    ]).

% handlers
-export([
    make_god/1
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the god library.
%% @end
%%----------------------------------------------------------------------
init() ->
    ok.

%%----------------------------------------------------------------------
%% @spec make_god(Pid) -> ok
%% where
%%      Name = list(),
%% @doc
%% Promotes a  player to god.
%% @end
%%----------------------------------------------------------------------
make_god(Pid) ->
    obj:call(Pid, transform, [god]),
    obj:call(Pid, set_max_speed, [100]),
    obj:call(Pid, set_property, [can_fly, true]),
    obj:call(Pid, set_mesh, [undefined]),
    obj:call(Pid, set_billboard, ["Abydos/Flare"]),
    obj:call(Pid, info),
    obj:call(Pid, post_init),
    obj:call(Pid, save).
    
