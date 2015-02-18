%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This module implements the player object which allows communication
%% with the players connection process in the connection server.
%% This object receives commands from the player connection process and
%% decides what to send back to the player.
%% @end
%%----------------------------------------------------------------------
-module(player).

%-behaviour(gen_client).
%-behaviour(gen_obj).

 %% @headerfile "obj.hrl"
-include("obj.hrl").
 %% @headerfile "vec.hrl"
-include("vec.hrl").
 %% @headerfile "protocol.hrl"
-include("protocol.hrl").
 %% @headerfile "conn_state.hrl"
-include("conn_state.hrl").
 %% @headerfile "shallow_object_info.hrl"
-include("shallow_object_info.hrl").

-define(PARENT, living).

-export([
    new/0,
    init/1,
    tick_chain/2,
    client_request/3,
    event_chain/4,
    command_chain/4,
    reply_chain/3
    ]).

% Exported functions used by modules higher up in the chain.
-export([
    client_reply/1
    ]).

new() ->
    {ok, Id} = libid_srv:generate_id(),
    {ok, #obj{id=Id, type=?MODULE}}.

init(#obj{id=Id} = State) ->
    {ok, NewState} = ?PARENT:init(State),
    {ok, NewState}.

tick_chain(LastTick, State) ->
    ?PARENT:tick_chain(LastTick, State).

command_chain(_From, set_conn, Conn, _State) ->
    obj:set_property(conn, Conn),
    noreply;

command_chain(_From, get_conn, [], _State) ->
    {reply, obj:get_property(conn)};

command_chain(_From, save, [], _State) ->
    error_logger:info_report({?MODULE, save}),
    {reply, obj:get_property(conn)};

command_chain(From, Function, Args, State) ->
    error_logger:info_report({?MODULE, Function, Args, State}),
    ?PARENT:command_chain(From, Function, Args, State).

event_chain(_From, set_mesh, {FromId, Mesh}, _State) ->
    IdBin = abydos_protocol:make_str(FromId),
    MeshBin = abydos_protocol:make_str(Mesh),
    %error_logger:info_report([{connsrv, playing, mesh, ?MESH, IdLen, Id, 
    %    MeshLen, MeshBin}]),
    client_reply(<<?MESH, IdBin/binary, MeshBin/binary>>);

event_chain(_From, set_dir, {FromId, Dir}, _State) ->
    IdBin = abydos_protocol:make_str(FromId),
    DirBin = abydos_protocol:make_vec(Dir),
    client_reply(<<?SET_DIR, IdBin/binary, DirBin/binary>>);

event_chain(_From, obj_leave, {id, Id}, _State) ->
    IdBin = abydos_protocol:make_str(Id),
    client_reply(<<?OBJ_LEAVE, IdBin/binary>>);

% This pattern match avoids handling the obj_enter message on
% ourself.
event_chain(_From, obj_enter, Id, #obj{id=Id}) ->
    pass;

event_chain(_From, obj_enter, TheirId, _State) ->
    TheirIdBin = abydos_protocol:make_str(TheirId),
    error_logger:info_report({?MODULE, obj_enter, client_reply, 
        TheirIdBin}),
    client_reply(<<?OBJ_ENTER, TheirIdBin/binary>>);

event_chain(From, Event, Args, State) ->
    ?PARENT:event_chain(From, Event, Args, State).

client_reply(Data) ->
    case obj:get_property(conn) of
        undefined ->
            error_logger:info_report({client_reply, conn_undefined});
        Conn when is_pid(Conn) ->
            Conn ! {client_reply, Data}
    end.

client_request(tcp_closed, #obj{id=Id}, _CharInfo) ->
    libplayer_srv:logout(Id),
    exit(normal);

client_request(<<?SET_DIR, X/little-float, Y/little-float, 
    Z/little-float>>, State, _CharInfo) ->
    {reply, #vec{x=X, y=Y, z=Z}} = command_chain(self(), set_dir, 
        {dir, #vec{x=X,y=Y,z=Z}}, State),
    client_reply(<<?SET_DIR, X/little-float, Y/little-float, 
        Z/little-float>>);

client_request(<<?GET_NAME, IdLen:8/integer, Id:IdLen/binary>>, 
    #obj{id=Id}, _CharInfo) ->
    Name = obj:get_property(<<"name">>),
    IdBinary = abydos_protocol:make_str(Id),
    NameBinary = abydos_protocol:make_str(Name),
    client_reply(<<?NOTIFY_NAME, IdBinary/binary, 
        NameBinary/binary>>);

client_request(<<?SAVE>>, #obj{id=Id} = State, 
    #conn_state{account=Account}) ->
    error_logger:info_report({?MODULE, saving, Id}),
    Name = obj:get_property(<<"name">>),
    libplayer_srv:save(Id, Account, Name, 
        State#obj{properties=obj:get_properties()}),
    {noreply, playing, State};

client_request(BinarySocketData, State, CharInfo) ->
    error_logger:info_report({unknown_client_request, 
        BinarySocketData, State, CharInfo}).

% Let's not send information about ourselves back to the client.
reply_chain(_From, #shallow_object_info{id=Id}, #obj{id=Id}) ->
    pass;

reply_chain(_From, {msg, Msg}, _State) ->
    MsgBin = abydos_protocol:make_str(Msg),
    client_reply(<<?MSG, MsgBin/binary>>);       
    
reply_chain(From, Reply, State) ->
    ?PARENT:reply_chain(From, Reply, State).

