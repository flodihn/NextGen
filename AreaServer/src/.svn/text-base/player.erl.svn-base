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

%% @docfile "doc/id.edoc"
%% @headerfile "obj.hrl"
-include("obj.hrl").
-include("vec.hrl").

-import(error_logger).


-import(libplayer).
-import(obj, [async_call/2, call_self/2, call_self/3]).

-export([
    create_state/1,
    init/1,
    post_init/2,
    update_parents/1,
    heart_beat/2,
    is/3,
    logout/2,
    pulse/2,
    pulse/3,
    query_entity/2,
    queried_entity/5,
    query_env/2,
    save/3,
    obj_created/3,
    obj_pos/4,
    obj_dir/5,
    obj_leave/3,
    obj_enter/3,
    obj_anim/4,
    obj_anim/5,
    obj_stop_anim/4,
    obj_speed/5,
    increase_speed/3,
    decrease_speed/3,
    set_dir/4,
    get_name/3,
    notify_flying/4,
    set_conn/3,
    get_conn/2,
    quad_changed/2,
    sync_pos/3,
    ping/3
    ]).

create_state(Type) ->
    {ok, State} = movable:create_state(Type),
    State2 = update_parents(State),
    {ok, _Reply, State3} = call_self(set_mesh, ["robot.mesh"], State2),
    {ok, _Reply, State4} = call_self(set_name, [<<"Unnamed soul">>], 
        State3),
    {ok, _Reply, State5} = call_self(set_heart_beat, [1000], State4),
    {ok, _Reply, State6} = call_self(set_max_speed, [10], State5),
    {ok, State6}.

%%----------------------------------------------------------------------
%% @spec init(State) ->{ok, NewState}
%% where
%%      State = obj()
%% @doc
%% Initiates the player. 
%% @end
%%----------------------------------------------------------------------
init(State) ->
    movable:init(State).

post_init(From, State) ->
    movable:post_init(From, State),
    %obj:async_call(self(), query_env),
    %obj:async_call(self(), pulse),
    {noreply, State}.

update_parents(State) ->
    State#obj{parents=[movable, obj]}.

heart_beat(From, State) ->
    %error_logger:info_report([{player, heart_beat}]),
    movable:heart_beat(From, State).

%%----------------------------------------------------------------------
%% @spec is(From, Type, State) ->{ok, true | false, State}
%% where
%%      From = pid(),
%%      Type = atom(),
%%      State = obj()
%% @doc
%% @see obj:is/3
%% @end
%%----------------------------------------------------------------------
is(_From, player, State) ->
    {reply, true, State};

is(From, Other, State) ->
    apply(movable, is, [From, Other, State]).

%%----------------------------------------------------------------------
%% @spec logout(From, State) -> ok
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Logout a player at once.
%% @end
%%----------------------------------------------------------------------
logout(_From, #obj{id=Id} = State) ->
    % This should be standard in the base obj.erl, also make objects exit
    % by sending stop msg to obj_loop.
    libstd.srv:unregister_obj(Id),
    {ok, Quad, _State} = call_self(get_quad, State),
    libtree.srv:handle_exit(Id, Quad),
    %error_logger:info_report([{player, Id, logout}]),
    exit(normal).

%%----------------------------------------------------------------------
%% @spec pulse(From, State) -> ok
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Send a pulse to all nearby objects, objects that are "hit" send their
%% graphical representation and position back to the object.
%% See also @see queried_entity/5
%% @end
%%----------------------------------------------------------------------
pulse(_From, State) ->
    %error_logger:info_report([{pulse_from, self()}]),
    call_self(event, [query_entity], State),
    %obj:event(self(), event, [test, ["foobar"]], State),
    %obj:event(self(), event, [test, ["foobar"]], State),
    {noreply, State}.

pulse(_From, Id, State) ->
    %error_logger:info_report([{"Pulsing object", Id}]),
    case libstd.srv:get_obj(Id) of
        {ok, _Id, Pid} ->
            async_call(Pid, query_entity);
        {error, no_obj} ->
            error_logger:error_report([{?MODULE, pulse, error, no_obj, Id}])
    end,
    {noreply, State}.

% Abort queries on ourself.
query_entity(From, State) when From == self() ->
    {noreply, State};

query_entity(From, State) ->
    movable:query_entity(From, State).

%----------------------------------------------------------------------
%% @spec queried_entity(From, {id, Id}, {key, Key}, {value, Value}, State) 
%% -> ok
%% where
%%      From = pid(),
%%      Id = id(),
%%      Key = string(),
%%      Value = any(),
%%      State = obj()
%% @doc
%% When an pulse hit an object it send makes an asyncronus call back to 
%% this object with this function, containging the queried properties. 
%% The properties are sent back to the connection.
%% @end
%%----------------------------------------------------------------------
queried_entity(_From, {id, Id}, {key, pos}, {value, Pos}, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "pos", Pos, Conn}]),
    Conn ! {new_pos, [Id, Pos]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, billboard}, {value, Billboard}, 
    State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "billboard", Billboard, 
    %    Conn}]),
    Conn ! {billboard, [Id, Billboard]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, mesh}, {value, Mesh}, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "mesh", Mesh, Conn}]),
    Conn ! {mesh, [Id, Mesh]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, dir}, {value, Dir}, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "dir", Dir, Conn}]),
    Conn ! {obj_dir, {id, Id}, {dir, Dir}}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, speed}, {value, Speed}, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "speed", Speed, Conn}]),
    Conn ! {obj_speed, {id, Id}, {speed, Speed}}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, anim}, {value, Anim}, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "anim", Anim, Conn}]),
    Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, 0}},
    {noreply, State};

queried_entity(_From, {id, Id}, {key, flying}, {value, true}, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "anim", Anim, Conn}]),
    Conn ! {{notify_flying, flying}, {id, Id}},
    {noreply, State}.

%%----------------------------------------------------------------------
%% @spec query_env(From, State) -> ok
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Query the skybox and terrain, returns the result to the players 
%% connection in the connection server.
%% @end
%%----------------------------------------------------------------------
query_env(_From, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    case libenv.srv:get_skybox() of
        {skybox, Skybox} ->
            Conn ! {skybox, Skybox};
        {error, _} ->
            pass
    end,
    case libenv.srv:get_terrain() of
        {terrain, Terrain} ->
            Conn ! {terrain, Terrain};
        {error, _} ->
            pass
    end,
    {noreply, State}.

save(_From, Account, #obj{id=Id} = State) ->
    {ok, Name, _State} = call_self(get_name, State),
    Result = libsave.srv:save_player(Id, Account, Name, State),
    {reply, Result, State}.

obj_created(_From, Id, #obj{id=MyId} = State) ->
    error_logger:info_report([{gaylord, obj_created, Id, MyId}]),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_created, {id, Id}},
    {noreply, State}.

obj_pos(_From, Id, Pos, State) ->
    %error_logger:info_report(obj_pos),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_pos, {id, Id}, {pos, Pos}},
    {noreply, State}.

obj_dir(_From, Id, Vec, TimeStamp, State) ->
    %error_logger:info_report([{State#obj.id, obj_dir, Id}]),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_dir, {id, Id}, {dir, Vec}, {timestamp, TimeStamp}},
    {noreply, State}.

obj_leave(From, _Id, State) when From == self() ->
    {noreply, State};

obj_leave(_From, Id, State) ->
    %error_logger:info_report(obj_leave),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_leave, {id, Id}},
    {noreply, State}.

obj_enter(_From, Id, State) ->
    %error_logger:info_report([{obj_enter, Id}]),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_enter, {id, Id}},
    {noreply, State}.

obj_anim(_From, Id, Anim, State) ->
    %error_logger:info_report(obj_anim),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, 0}},
    {noreply, State}.


obj_anim(_From, Id, Anim, Nr, State) ->
    %error_logger:info_report(obj_anim),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, Nr}},
    {noreply, State}.

obj_stop_anim(_From, Id, Anim, State)->
    %error_logger:info_report(obj_stop_anim),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_stop_anim, {id, Id}, {anim, Anim}},
    {noreply, State}.

obj_speed(_From, Id, Speed, TimeStamp, State) ->
    %&error_logger:info_report([{test, obj_speed}]),
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {obj_speed, {id, Id}, {speed, Speed}, {timestamp, TimeStamp}},
    {noreply, State}.

increase_speed(_From, TimeStamp, #obj{id=Id} = State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    {ok, MaxSpeed, _State} = obj:call_self(get_max_speed, State),
    NewSpeed = OldSpeed + 1,
    case NewSpeed > MaxSpeed of
        true ->
            {noreply, State};
        false ->  
            obj:call_self(event, [obj_anim, [Id, "Walk"]], State),
            {ok, _Reply, NewState} = obj:call_self(set_speed, [NewSpeed,
                TimeStamp], State),
            {noreply, NewState}
    end.

decrease_speed(_From, TimeStamp, #obj{id=Id} = State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    NewSpeed = OldSpeed - 1,
    case NewSpeed of
        0 ->
            %obj:call_self(event, [obj_stop_anim, [Id, "Walk"]], State);
            obj:call_self(event, [obj_anim, [Id, "Idle"]], State);
        _Any ->
            pass
    end, 
    case OldSpeed > 0 of
        true -> 
            {ok, _Reply, NewState} = obj:call_self(set_speed, [NewSpeed,
                TimeStamp], State),
            % event is called in set_speed.
            %call_self(event, [obj_speed, [Id, NewSpeed, TimeStamp]], 
            %    NewState),
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

set_dir(From, Dir, TimeStamp, State) ->
    obj:set_dir(From, Dir#vec{y=0}, TimeStamp, State).

get_name(_From, Id, #obj{id=Id} = State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    case obj:call_self(get_name, State) of
        {ok, undefined, _State} -> 
                pass;
        {ok, Reply, _State} -> 
            Conn ! {notify_name, {id, Id}, {name, Reply}}
    end,
    {noreply, State}.

notify_flying(_From, Id, Mode, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    Conn ! {{notify_flying, Mode}, {id, Id}},
    {noreply, State}.

ping(_From, Time, State) ->
    {ok, Conn, _State} = call_self(get_conn, State),
    %call_self(event, [foobar], State),
    Conn ! {pong, Time},
    {noreply, State}.

set_conn(_From, Conn, State) ->
    {ok, _Reply, NewState} = obj:call_self(set_property, [conn, Conn], 
        State),
    {noreply, NewState}.

get_conn(_From, State) ->
    {ok, Conn, _State} = obj:call_self(get_property, [conn], 
        State),
    {reply, Conn, State}.

quad_changed(_From, State) ->
    obj:call_self(pulse, State),
    {noreply, State}.

% For now we trust the client updating our position, this should be 
% changed when the servers is aware of the terrain.
sync_pos(_From, Pos, State) ->
    error_logger:info_report([{sync_pos, Pos}]),
    {ok, _Reply, NewState} = obj:call_self(set_pos, [Pos], State), 
    {noreply, NewState}.

