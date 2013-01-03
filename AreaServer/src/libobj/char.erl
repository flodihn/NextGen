-module(char).

-include("char.hrl").
-include("obj_state.hrl").

-export([
    start_link/1,
    init/1,
    loop/1,
    pulse/1
    ]).

start_link(CharState) ->
    Pid = spawn_link(?MODULE, init, [CharState]),
    {ok, Pid}.

init(#char{conn=Conn} = CharState) ->
    {ok, NewObjState} = obj:init(CharState#char.obj_state),
	Conn ! {set_char_pid, self()},
    Terrain = libenv:get_terrain(),
    Conn ! Terrain,
    Skybox = time_daemon:get_skybox(),
    Conn ! Skybox,
    AmbientLight = time_daemon:get_ambient_light(),
    Conn ! AmbientLight,
    {ok, NewObjState2} = obj:set_property(billboard, "Abydos/Flare", 
        NewObjState),
    %error_logger:info_report([{NewObjState2}]),
    ?MODULE:loop(CharState#char{obj_state=NewObjState2}).

pulse(From) ->
    std_funs:area_event({From, query_entity}).

loop(#char{conn=Conn, obj_state=ObjState} = State) ->
    receive 
        %{ambient_light, AmbienLight} ->
        %    Conn ! {ambient_light, AmbienLight},
        %    ?MODULE:loop(State);
        %{skybox, SkyBox} ->
        %    Conn ! {skybox, SkyBox},
        %    ?MODULE:loop(State);
        {new_time_of_day, DayTime} ->
            AmbientLight = time_daemon:get_ambient_light(DayTime),
            SkyBox = time_daemon:get_skybox(DayTime),
            Conn ! SkyBox,
            Conn ! AmbientLight,
            ?MODULE:loop(State);
        {move_obj, Id, Pos} ->
            error_logger:info_report([{moving_obj, Id, Pos}]),
            {ok, {obj_registry, Id, Pid}} = std_funs:get_obj(Id),
            Pid ! {move, Pos},
            Pid ! save,
            ?MODULE:loop(State);
        {set_mesh, Id, Mesh} ->
            {ok, {obj_registry, Id, Pid}} = std_funs:get_obj(Id),
            Pid ! {set_mesh, Mesh},
            Pid ! save,
            ?MODULE:loop(State);
        {Id, new_pos, Pos} ->
            Conn ! {new_pos, Id, Pos},
            ?MODULE:loop(State);
        {create_obj, Mesh, {X, Y, Z}} ->
            error_logger:info_report([{create_obj, Mesh, X, Y, Z}]),
            {ok, Pid} = obj_sup:start(),
            Pid ! {set_pos, {X, Y, Z}},
            Pid ! {set_mesh, Mesh},
            Pid ! save,
            ?MODULE:loop(State);
        logout ->
            obj:destroy(ObjState);
        {mesh, Id, Mesh} ->
            Conn ! {mesh, Id, Mesh},
            ?MODULE:loop(State);
        {From, pulse} ->
            char:pulse(From),
            ?MODULE:loop(State);
        {From, query_entity} ->
            obj:query_entity(From, ObjState),
            ?MODULE:loop(State);
        {billboard, Id, Billboard} ->
            error_logger:info_report([{billboard, Id, Billboard}]),
            Conn ! {billboard, Id, Billboard},
            ?MODULE:loop(State);
        {From, get_billboard} ->
            Billboard = obj:get_property(billboard, ObjState),
            From ! {billboard, Billboard},
            ?MODULE:loop(State);
        {From, get_id} ->
            From ! {id, ObjState#obj_state.id},
            ?MODULE:loop(State);
        {migrate, Area} ->
            obj:migrate(Area, ?MODULE, ObjState, State);
        {node_migrate, _Node} ->
            ignore, % Dont migrate players...
            ?MODULE:loop(State);
        {sync_y_move, Pos} ->
            {ok, NewObjState} = obj:sync_y_move(Pos, ObjState),
            error_logger:info_report([{Pos}]),
            ?MODULE:loop(State#char{obj_state=NewObjState});
        {move, Pos} ->
            NewObjState = obj:move(Pos, ObjState),
            ?MODULE:loop(State#char{obj_state=NewObjState});
        {Id, move, Pos} ->
            Conn ! {new_pos, Id, Pos},
            ?MODULE:loop(State);
        {spam_from, From} ->
            Conn ! {msg, integer_to_list(random:uniform(100)) ++ " SPAM: " ++ atom_to_list(node(From))},
           ?MODULE:loop(State);
        {send_msg, Msg} ->
            std_funs:area_event({msg, Msg}),
            ?MODULE:loop(State);
        {msg, Msg} ->
            Conn ! {msg, Msg},
            ?MODULE:loop(State);
        Event ->
            %error_logger:info_report([{debug, char, handle, Event}]),
            Reply = obj:handle(Event),
            Conn ! Reply,
            ?MODULE:loop(State)
    after 10000 ->
        ?MODULE:loop(State)
    end.

