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

% Internal exports.
-export([
    async_create/1,
    async_login/2
    ]).

% handlers
-export([
    create/1,
    login/2,
    save/2
    ]).

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
    spawn(?MODULE, async_create, [Conn]).

async_create(Conn) ->
    %error_logger:info_report([{?MODULE, async_create, Conn}]),
    {ok, Pid} = obj_sup:start(player),
    X = rand:int(1, 10000),
    Y = 0,
    Z = rand:int(1, 10000),
    %%X = 4886,
    %%Y = 494,
    %%Z = 5853,
    obj:call(Pid, set_conn, [Conn]),
    {ok, Id} = obj:call(Pid, get_id, []),
    %%{ok, Pos} = obj:call(Pid, get_pos),
    %%Conn ! {new_pos, [Id, Pos]},
    %%{ok, CharSrv} = application:get_env(charsrv),
    %%case rpc:call(CharSrv, charsrv, get_char_count, []) of
    %%    0 ->
    %%        libgod.srv:make_god(Pid);
    %%    _NotZero ->
    %%        pass
    %%end,
    obj:call(Pid, set_pos, [#vec{x=X, y=Y, z=Z}]),
    obj:async_call(Pid, post_init),
    Conn ! {char_login, {pid, Pid}, {id, Id}}.
    
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
    spawn(?MODULE, async_login, [Conn, Id]).

async_login(Conn, Id) ->
    {ok, CharSrv} = application:get_env(charsrv),
    {ok, State} = rpc:call(CharSrv, charsrv, load, [Id]),
    {ok, Pid} = obj_sup:start(player, State),
    obj:call(Pid, set_conn, [Conn]),
    obj:call(Pid, post_init),
    
    % These lines should not be needed.
    {ok, Pos} = obj:call(Pid, get_pos),
    Conn ! {new_pos, [Id, Pos]},

    error_logger:info_report([{player, Id, logged_in, Pid}]),
    Conn ! {char_login, {pid, Pid}, {id, Id}}.

%% @private
unregister_events() ->
    %areasrv:remove_handler(char_login).
    ok.

save(Id, State) ->
    {ok, CharSrv} = application:get_env(charsrv),
    %io:format("Saving player ~p at node ~p.~n", [State, CharSrv]),
    rpc:call(CharSrv, charsrv, save, [Id, State]).



