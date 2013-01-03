-module(old_obj).
-include("obj_state.hrl").

-include("vec.hrl").

-export([
    start_link/1,
    start_link/2,
    init/1,
    migrate/4,
    node_migrate/4,
    handle/1,
    set_property/3,
    get_property/2,
    set_short/2,
    get_short/1,
    set_long/2,
    get_long/1,
    move/2,
    set_billboard/2,
    set_mesh/2,
    query_entity/2,
    set_scale/2,
    save/1,
    destroy/1,
    sync_y_move/2
    ]).

% Internal exports
-export([
        loop/1,
        loop_init/1
    ]).


% Debugging exports
-export([
    check_pos/1
    ]).

% Export the behaviour
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {start_link, 1}, 
        {init, 1},
        {migrate, 1}
    ];

behaviour_info(_Other) ->
	undefined.

start_link(ObjState) ->
    Pid = spawn_link(?MODULE, loop_init, [ObjState]),
    {ok, Pid}.

% This is pretty insane, each type should be responsible for
% creating themselves. Have to fix later.
start_link(Type, TypeState) ->
    Type:start_link(TypeState).

init(#obj_state{id=Id} = State) when Id == undefined ->
    NewId = std_funs:generate_id(?MODULE),
    %Pos = #vec{x=1500, y=500, z=1500},
    Pos = #vec{x=1500, y=286, z=1500},
    init(State#obj_state{id=NewId, pos=Pos});

init(#obj_state{id=Id, properties=Properties} = State) when 
    Properties == undefined ->
    error_logger:info_report([{creating_new_dict, Id}]),
    init(State#obj_state{properties=dict:new()});

init(#obj_state{id=Id} = State) ->
	case std_funs:register_obj(Id, self()) of
        {ok, _Id} ->
            dist_funs:register_obj(Id, self()),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

migrate(Area, Type, State, TypeState) ->
    Node = std_funs:monsrv_rpc(mon, get_area, [Area]),
	migrate:migrate(Node, Type, State, TypeState).

node_migrate(Node, Type, State, TypeState) ->
	migrate:migrate(Node, Type, State, TypeState).

sync_y_move({X, Y, Z}, State) ->
    Pos = #vec{x=X, y=Y, z=Z},
	NewPos = util:vector_add(State#obj_state.pos, Pos),
    NewCorrectedPos = NewPos#vec{y=Y},
    %error_logger:info_report([{NewCorrectedPos}]),
    std_funs:area_event({State#obj_state.id, move, NewCorrectedPos}),
    {ok, State#obj_state{pos=NewCorrectedPos}}.

move({X, Y, Z}, State) ->
    Pos = #vec{x=X, y=Y, z=Z},
	NewPos = util:vector_add(State#obj_state.pos, Pos),
    
    % Bug in here, area some how get an ok atom
    %case check_pos(NewPos) of
    %    false ->
    %        State#obj_state{pos=NewPos};
    %    {Area, Border}  ->
    %        NewAreaVec = geo_funs:calc_new_pos(State#obj_state.pos, Border),
    %        self() ! {migrate, Area},
    %        State#obj_state{pos=NewAreaVec}
    %end.
    std_funs:area_event({State#obj_state.id, move, NewPos}),
    State#obj_state{pos=NewPos}.

check_pos(NewPos) ->
    case geo_funs:query_area(NewPos) of
        {error, Reason} ->
            error_logger:error_report([{obj, move, error, Reason}]),
            false;
        {Area, Border} ->
            % If it is a new area we migrate to it. 
            case std_funs:area_name() == Area of
                true ->
                    error_logger:info_report(
                        [{check_pos, old_area, Area}]),
                    false;
                false ->
                    error_logger:info_report(
                        [{check_pos, new_area, Area}]),
                    {Area, Border}
            end
    end.

handle(Event) ->
    EventId = areasrv:event(Event),
	event:gather(EventId).

%Make properties a gb_tree instead of dict?
set_property(Property, Value, State) ->
    Properties = State#obj_state.properties,
    %NewProperties = dict:append(Property, Value, Properties),
    NewProperties = dict:store(Property, Value, Properties),
    {ok, State#obj_state{properties=NewProperties}}.

get_property(Property, State) ->
    dict:find(Property, State#obj_state.properties).

set_short(Desc, State) ->
    set_property(short, Desc, State).

get_short(State) ->
    get_property(short, State).   

set_long(Desc, State) ->
    set_property(short, Desc, State).

get_long(State) ->
    get_property(long, State).

loop_init(State) ->
    {ok, NewState} = init(State),
    ?MODULE:loop(NewState).

set_billboard(Billboard, State) ->
    std_funs:area_event({billboard, State#obj_state.id, Billboard}),
    set_property(billboard, Billboard, State).

query_entity(From, State) ->
    case get_property(mesh, State) of
        error ->
            pass;
        {ok, Mesh} ->
            From ! {mesh, State#obj_state.id, Mesh}
    end,
    case get_property(billboard, State) of
        error ->
            pass;
        {ok, Billboard} ->
            From ! {billboard, State#obj_state.id, Billboard}
    end,
    case get_property(scale, State) of
        error ->
            pass;
        {ok, Scale}  ->
            From ! {scale, State#obj_state.id, Scale}
    end,
    case State#obj_state.pos of
        undefined ->
            pass;
        Vec ->
            From ! {new_pos, State#obj_state.id, Vec}
    end.

set_mesh(Mesh, State) ->
    std_funs:area_event({mesh, State#obj_state.id, Mesh}),
    set_property(mesh, Mesh, State).

set_scale(Scale, State) ->
    std_funs:area_event({scale, State#obj_state.id, Scale}),
    set_property(scale, Scale, State).

save(State) ->
    libsave:save(State#obj_state.id, State).

destroy(#obj_state{id=Id}) ->
    error_logger:info_report([{unregistering_and_destroying, Id}]),
	std_funs:unregister_obj(Id),
    exit(normal).

set_pos({X, Y, Z}, State) ->
    NewPos = #vec{x=X, y=Y, z=Z},
    std_funs:area_event({State#obj_state.id, new_pos, NewPos}),
    error_logger:info_report([{State#obj_state.id, set_pos, NewPos}]),
    {ok, State#obj_state{pos=NewPos}}.

loop(State) ->
    receive 
        {set_pos, Pos} ->
            {ok, NewState} = set_pos(Pos, State),
	        ?MODULE:loop(NewState);
        {_Id, new_pos, _Pos} ->
	        ?MODULE:loop(State);
        save ->
            save(State),
	        ?MODULE:loop(State);
        {From, query_entity} ->
            query_entity(From, State),
	        ?MODULE:loop(State);
	    debug ->
	        error_logger:info_report([State]),
	        ?MODULE:loop(State);
        {spam_from, _From} ->
            ignore,
            ?MODULE:loop(State);  
        {migrate, Area} ->
            migrate(Area, ?MODULE, State, State);
        % node_migrate is for testing purpose.
        {node_migrate, Node} ->
            node_migrate(Node, ?MODULE, State, State);
        {set_billboard, Billboard} ->
            %error_logger:info_report([{obj, set_billboard, Billboard}]),
            {ok, NewState} = obj:set_billboard(Billboard, State),
	        ?MODULE:loop(NewState);
        {set_mesh, Mesh} ->
            %error_logger:info_report([{obj, set_mesh, Mesh}]),
            {ok, NewState} = obj:set_mesh(Mesh, State),
	        ?MODULE:loop(NewState);
        {set_scale, Scale} ->
            %error_logger:info_report([{obj, set_scale, Scale}]),
            {ok, NewState} = obj:set_scale(Scale, State),
	        ?MODULE:loop(NewState);
	    {move, Pos} ->
            %error_logger:info_report([{move, self()}]),
            NewState = move(Pos, State),
            ?MODULE:loop(NewState);  
        Event ->
            %error_logger:info_report([{obj, self(), Event}]),
			handle(Event),
            ?MODULE:loop(State)
    after 10000 ->
        ?MODULE:loop(State)
    end.

    
