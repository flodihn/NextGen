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
    log/1
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the logger.
%% @end
%%----------------------------------------------------------------------
init() ->
    {ok, []}.

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
log(_Data) ->
	ok.
