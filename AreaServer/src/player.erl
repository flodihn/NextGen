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
    obj_stop_anim/4,
    obj_speed/5,
    obj_dead/3,
    obj_logout/3,
	obj_jump/4,
	obj_vector/4,
	obj_shot/4,
	obj_respawn/4,
	obj_faction/4,
	obj_jump_slam_attack/5,
    increase_speed/3,
    decrease_speed/3,
    set_dir/4,
    get_name/3,
    notify_flying/4,
    set_conn/3,
    get_conn/2,
    quad_changed/2,
    sync_pos/4,
    ping/3,
	set_shot/4,
	set_faction/3,
	set_respawn/2,
	set_anim/3,
	set_jump_slam_attack/4,
	entity_interpolation/5
    ]).

create_state(Type) ->
    {ok, State} = movable:create_state(Type),
    State2 = update_parents(State),
    {noreply, State3} = obj:set_property(self(), last_heart_beat, now(),
		State2),
    {ok, _Reply, State4} = obj:call_self(set_heart_beat, [10000], State3),
    {ok, State4}.

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

post_init(From, #obj{id=Id} = State) ->
   {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_enter, {id, Id}},
	case obj:call_self(get_property, [faction], State) of
    	{ok, undefined, _} ->
       		pass;
      	{ok, Faction, _} ->
   			Conn ! {obj_faction, {id, Id}, {faction, Faction}},
         	obj:async_call(From, queried_entity, [{id, Id}, 
           	{key, faction}, {value, Faction}])
   	end,
	case obj:call_self(get_pos, State) of
		{ok, Pos, _State} ->
    		Conn ! {obj_pos, {id, Id}, {pos, Pos}};
		_Error ->
			pass
	end,
    obj:async_call(self(), pulse),
    movable:post_init(From, State),
    {noreply, State}.

update_parents(State) ->
    State#obj{parents=[movable, obj]}.

heart_beat(From, State) ->
	obj:quadtree_assign(self(), State),
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
    obj:call_self(event, [obj_logout, [Id]], State),
    libstd_srv:unregister_obj(Id),
	liblog_srv:clear_log(Id),
    {ok, Quad, _State} = obj:call_self(get_quad, State),
    libtree_srv:handle_exit(Id, Quad),
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
pulse(From, State) ->
	Self = self(),
	case From of
		Self ->
    		error_logger:info_report("ignore_pulse_from_self"),
			pass;
		_OtherPid ->
    		obj:call_self(event, [query_entity], State)
	end,
    {noreply, State}.

% Ignore pulse messages from ourselves.
pulse(_From, Id, #obj{id=Id} = State) ->
    {noreply, State};

pulse(_From, Id, State) ->
    %error_logger:info_report([{"Pulsing object", Id}]),
    case libstd_srv:get_obj(Id) of
        {ok, _Id, Pid} ->
            obj:async_call(Pid, query_entity);
        {error, no_obj} ->
            error_logger:error_report([{?MODULE, pulse, error, no_obj, Id}])
    end,
    {noreply, State}.

set_shot(_From, Id, ShotPos, #obj{id=MyId} = State) when Id /= MyId->
	case Id of
        <<>> ->
            pass;
		_ValidId ->
            obj:event(self(), event, [obj_dead, [Id]], State)
    end,
    obj:event(self(), event, [obj_shot, [MyId, ShotPos]], State),
    {noreply, State};

set_shot(_From, Id, _ShotPos, #obj{id=Id} = State) ->
	error_logger:info_report({?MODULE, set_shot, can_not_shoot_self, Id}),
    {noreply, State}.

query_entity(From, #obj{id=Id} = State) ->
	Self = self(),
	case From of
		Self ->
			{noreply, State};
		_Other ->
			case obj:call_self(get_property, [faction], State) of
       			 {ok, undefined, _} ->
       			     pass;
      			 {ok, Faction, _} ->
            		obj:async_call(From, queried_entity, [{id, Id}, 
                		{key, faction}, {value, Faction}])
    		end,
    		movable:query_entity(From, State)
	end.

obj_logout(_From, Id, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_logout, {id, Id}},
    {noreply, State}.

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
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "pos", Pos, Conn}]),
    Conn ! {new_pos, [Id, Pos]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, billboard}, {value, Billboard}, 
    State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "billboard", Billboard, 
    %    Conn}]),
    Conn ! {billboard, [Id, Billboard]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, mesh}, {value, Mesh}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "mesh", Mesh, Conn}]),
    Conn ! {mesh, [Id, Mesh]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, dir}, {value, Dir}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "dir", Dir, Conn}]),
    Conn ! {obj_dir, {id, Id}, {dir, Dir}}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, speed}, {value, Speed}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "speed", Speed, Conn}]),
    Conn ! {obj_speed, {id, Id}, {speed, Speed}}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, flying}, {value, true}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    %error_logger:info_report([{queried_entity, Id, "anim", Anim, Conn}]),
    Conn ! {{notify_flying, flying}, {id, Id}},
    {noreply, State};

queried_entity(_From, {id, Id}, {key, faction}, {value, Faction}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_faction, {id, Id}, {faction, Faction}},
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
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    case libenv_srv:get_skybox() of
        {skybox, Skybox} ->
            Conn ! {skybox, Skybox};
        {error, _} ->
            pass
    end,
    case libenv_srv:get_terrain() of
        {terrain, Terrain} ->
            Conn ! {terrain, Terrain};
        {error, _} ->
            pass
    end,
    {noreply, State}.

save(_From, Account, #obj{id=Id} = State) ->
    {ok, Name, _State} = obj:call_self(get_name, State),
    Result = libsave_srv:save_player(Id, Account, Name, State),
    {reply, Result, State}.

obj_created(_From, Id, #obj{id=MyId} = State) ->
    error_logger:info_report([{obj_created, Id, MyId}]),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_created, {id, Id}},
    {noreply, State}.

obj_pos(_From, Id, Pos, State) ->
    %error_logger:info_report(obj_pos),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_pos, {id, Id}, {pos, Pos}},
    {noreply, State}.

obj_dir(_From, Id, Vec, TimeStamp, State) ->
    %error_logger:info_report([{State#obj.id, obj_dir, Id}]),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_dir, {id, Id}, {dir, Vec}, {timestamp, TimeStamp}},
    {noreply, State}.

obj_leave(From, _Id, State) when From == self() ->
    {noreply, State};

obj_leave(_From, Id, State) ->
    %error_logger:info_report(obj_leave),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_leave, {id, Id}},
    {noreply, State}.

obj_enter(_From, Id, State) ->
    %error_logger:info_report([{obj_enter, Id}]),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_enter, {id, Id}},
    {noreply, State}.

obj_anim(_From, Id, AnimStr, State) ->
    %error_logger:info_report(obj_anim),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_anim, {id, Id}, {animstr, AnimStr}},
    {noreply, State}.

obj_stop_anim(_From, Id, Anim, State)->
    %error_logger:info_report(obj_stop_anim),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_stop_anim, {id, Id}, {anim, Anim}},
    {noreply, State}.

obj_speed(_From, Id, Speed, TimeStamp, State) ->
    %error_logger:info_report([{test, obj_speed}]),
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_speed, {id, Id}, {speed, Speed}, {timestamp, TimeStamp}},
    {noreply, State}.

obj_dead(_From, Id, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_dead, {id, Id}},
    {noreply, State}.

obj_jump(_From, Id, Force, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_jump, {id, Id}, {force, Force}},
    {noreply, State}.

obj_vector(_From, Id, Vector, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_vector, {id, Id}, {velocity, Vector}},
    {noreply, State}.

obj_shot(_From, Id, ShotPos, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_shot, {id, Id}, {shot_pos, ShotPos}},
    {noreply, State}.

obj_respawn(_From, Id, {pos, NewPos}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_respawn, {id, Id}, {pos, NewPos}},
    {noreply, State}.

obj_faction(_From, Id, {faction, Faction}, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_faction, {id, Id}, {faction, Faction}},
    {noreply, State}.

obj_jump_slam_attack(_From, Id, Str, Vec, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {obj_jump_slam_attack, {id, Id}, {str, Str}, {vec, Vec}},
    {noreply, State}.

% Dont send the interpolation message to ourselves.
entity_interpolation(_From, Id, _Pos, _Dir, #obj{id=Id} = State) ->
	{noreply, State};

entity_interpolation(_From, Id, Pos, Dir, #obj{id=_OtherId} = State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {entity_interpolation, {id, Id}, {pos, Pos}, {dir, Dir}},
	{noreply, State}.

increase_speed(_From, TimeStamp, State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    %{ok, MaxSpeed, _State} = obj:call_self(get_max_speed, State),
    NewSpeed = OldSpeed + 1,
    {ok, _Reply, NewState} = obj:call_self(set_speed, [NewSpeed,
    	TimeStamp], State),
    {noreply, NewState}.

decrease_speed(_From, TimeStamp, State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    NewSpeed = OldSpeed - 1,
    case NewSpeed of
        0 ->
			pass;
        _Any ->
            pass
    end, 
    case OldSpeed > 0 of
        true -> 
            {ok, _Reply, NewState} = obj:call_self(set_speed, [NewSpeed,
                TimeStamp], State),
            {noreply, NewState};
        false ->
            {noreply, State}
    end.

set_dir(From, Dir, TimeStamp, #obj{id=Id} = State) ->
    obj:set_dir(From, Dir, TimeStamp, State),
    obj:call_self(event, [obj_dir, [Id, Dir, TimeStamp]], State),
	{noreply, State}.

get_name(_From, Id, #obj{id=Id} = State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    case obj:call_self(get_name, State) of
        {ok, undefined, _State} -> 
                pass;
        {ok, Reply, _State} -> 
            Conn ! {notify_name, {id, Id}, {name, Reply}}
    end,
    {noreply, State}.

notify_flying(_From, Id, Mode, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {{notify_flying, Mode}, {id, Id}},
    {noreply, State}.

ping(_From, Time, State) ->
    {ok, Conn, _State} = obj:call_self(get_conn, State),
    Conn ! {pong, Time},
    {noreply, State}.

set_conn(_From, Conn, State) ->
    {ok, _Reply, NewState} = obj:call_self(
		set_property, [conn, Conn], State),
    {noreply, NewState}.

get_conn(_From, State) ->
    {ok, Conn, _State} = obj:call_self(
		get_property, [conn], State),
    {reply, Conn, State}.

quad_changed(_From, State) ->
	error_logger:info_report({quad_changed, State#obj.id}),
    obj:call_self(pulse, State),
    {noreply, State}.

set_faction(_From, Faction, State) ->
	{ok, noreply, NewState} = obj:call_self(
		set_property, [faction, Faction], State),
	{noreply, NewState}.

set_respawn(_From, #obj{id=Id} = State) ->
	{ok, {faction, Faction}} = libfaction_srv:assign(),
	{ok, SpawnPoint} = libfaction_srv:get_spawn_point(Faction),
    obj:call_self(event, [obj_respawn, [Id, {pos, SpawnPoint}]], State),
	{noreply, State}.
	
set_anim(_From, AnimStr, #obj{id=Id} = State) ->
    obj:call_self(event, [obj_anim, [Id, AnimStr]], State),
	{noreply, State}.

set_jump_slam_attack(_From, Str, Vec, #obj{id=Id} = State) ->
    obj:call_self(event, [obj_jump_slam_attack, [Id, Str, Vec]], State),
	{noreply, State}.

% For now we trust the client updating our position, this should be 
% changed when the servers is aware of the terrain.
sync_pos(_From, Pos, Dir, #obj{id=Id} = State) ->
	liblog_srv:log({sync_pos, {id, Id}, {log, Pos}}),
	obj:quadtree_assign(self(), State),
    obj:call_self(event, [entity_interpolation, [Id, Pos, Dir]], State),
    {ok, _Reply, NewState} = obj:call_self(set_pos, [Pos], State), 
    {noreply, NewState}.

