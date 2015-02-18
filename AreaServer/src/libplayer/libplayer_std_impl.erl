%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the standard implementation for the player library 'libplayer'.
%% The module provides functions to create and login players.
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(libplayer_std_impl).

-include("char.hrl").
-include("vec.hrl").

% API
-export([
    init/0,
    unregister_events/0
    ]).

% handlers
-export([
    create/1,
    login/2,
    save/4
    ]).

-define(START_POS, #vec{x=117, y=287.5, z=280.0}).
-define(DEFAULT_NAME, <<"Noname">>).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the player library.
%% @end
%%----------------------------------------------------------------------
init() ->
    %{ok, AreaNode} = application:get_env(area_node),
    %rpc:call(AreaNode, areasrv, add_handler, 
    %    [login_char, node(), libplayer, event]),
    %areasrv:add_handler(char_login, libchar),
    %ets:new(players, [named_table]),
    ok.

%%----------------------------------------------------------------------
%% @spec create(Conn) -> {ok, {pid, Pid}, {id, Id}}
%% where
%%      Conn = pid(),
%%      Pid = pid(),
%%      Id = id()
%% @doc
%% Creates a new player object and returns its pid and id.
%% @end
%%----------------------------------------------------------------------
create(Conn) ->
    {ok, Pid} = obj_sup:new(player, [
        {conn, Conn},
        {<<"pos">>, ?START_POS},
        {<<"name">>, ?DEFAULT_NAME}]),
    {ok, Id} = obj:trigger_sync_command_chain(Pid, get_id, []),
    obj:set_tick(Pid, 2000),
    % TODO: This should be stored on a shared mnesia table on shared server,
    % not in an ets table.
    %ets:insert(players, {Id, {?DEFAULT_NAME, Pid}}), 
    error_logger:info_report([{player, Id, logged_in, Pid}]),
    {ok, {pid, Pid}, {id, Id}}.
    
%%----------------------------------------------------------------------
%% @doc
%% @spec login(State) ->{ok, Id, Pid}
%% where
%%      State = obj_state(), 
%%      Id = string(),
%%      Pid = pid() 
%% @type obj_state(). An obj_state record.
%% Purpose: Makes a player login.
%% @end
%%----------------------------------------------------------------------
login(Conn, Id) ->
    {ok, CharSrv} = application:get_env(charsrv),
    {ok, State} = rpc:call(CharSrv, charsrv, load, [Id]),
    {ok, Pid} = obj_sup:inst(State, [{conn, Conn}]),
    error_logger:info_report([{player, Id, logged_in, Pid}]),
    {ok, {pid, Pid}, {id, Id}}.

%% @private
unregister_events() ->
    %areasrv:remove_handler(char_login).
    ok.

save(Id, Account, Name, ObjState) ->
    {ok, CharSrv} = application:get_env(charsrv),
    R = rpc:call(CharSrv, charsrv, save, [Id, Account, Name, ObjState]),
    error_logger:info_report({"Saving player", State, CharSrv, result, R}).


