%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% @end
%%----------------------------------------------------------------------
-module(living).

 %% @headerfile "obj.hrl"
-include("obj.hrl").

-define(PARENT, movable).

-export([
    new/0,
    init/1,
    tick_chain/2,
    event_chain/4,
    command_chain/4,
    reply_chain/3
    ]).

new() ->
    ?PARENT:new().

init(State) ->
    ?PARENT:init(State).

tick_chain(LastTick, State) ->
    ?PARENT:tick_chain(LastTick, State).

command_chain(From, Function, Args, State) ->
    ?PARENT:command_chain(From, Function, Args, State).

event_chain(From, Event, Args, State) ->
    ?PARENT:event_chain(From, Event, Args, State).

reply_chain(From, Reply, State) ->
    ?PARENT:reply_chain(From, Reply, State).

