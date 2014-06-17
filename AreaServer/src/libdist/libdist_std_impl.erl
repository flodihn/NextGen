-module(libdist_std_impl).

-include("obj_registry.hrl").
-include("shared_obj_registry.hrl").
-include("areas.hrl").

% API
-export([
    init/0,
    create_area/0,
    join_area/0,
    register_node/0,
    unregister_node/0,
    register_obj/2,
    unregister_obj/1,
    get_obj/1,
    next_obj/0,
    next_obj/1,
    find_obj/1
    ]).

% internal exports
-export([
    ]).

init() ->
    mnesia:start(),
    % The the shared_obj_registry exists we are part of an area and
    % should register ourself in the monitor server.
    case lists:member(shared_obj_registry, 
        mnesia:system_info(tables)) of
        true ->
            %std_funs:monsrv_rpc(mon, register_node, [node()]);
            pass;
        false ->
            nop
    end.

register_node() ->
    Name = libstd_srv:area_name(),
    F = fun() ->
        {atomic, Area} = mnesia:read(areas, Name),
        NewNodes = lists:append(Area#areas.nodes, [node()]),
        mnesia:write(areas, Name, Area#areas{nodes=NewNodes}) 
    end,
    mnesia:transaction(F).

unregister_node() ->
    ok.

% Only called once per area.
create_area() ->
    libdist_balancer:create_table(),
    mnesia:create_table(shared_obj_registry, 
        [{ram_copies, [node()]},
        {attributes, record_info(fields, shared_obj_registry)}]).

join_area() ->
    Node = libstd_srv:monsrv_rpc(monsrv, get_area, 
        [libstd_srv:area_name()]),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    libdist_balancer:join(Node),
    rpc:call(Node, mnesia, add_table_copy, 
        [shared_obj_registry, node(), ram_copies]),
    % The obj_registry table has the {local_content, true} option. So
    % we can safely add it to our shared schema but the data will still
    % be local.
    rpc:call(Node, mnesia, add_table_copy, 
        [obj_registry, node(), ram_copies]),
    % These should rather be called through an event handler, fix later.
    libtree_srv:join_area(Node),
    libenv_srv:join_area(Node),
    libsave_srv:join_area(Node),
    libstd_srv:monsrv_rpc(monsrv, register_node, [node()]),
    Node.

register_obj(undefined, Pid) ->
    register_obj(make_ref(), Pid);

register_obj(AreaId, Pid) ->
    mnesia:transaction(
        fun() -> 
            mnesia:write(#shared_obj_registry{ref=AreaId,  pid=Pid})
        end).

unregister_obj(AreaId) ->
    mnesia:transaction(
        fun() ->
            mnesia:delete({shared_obj_registry, AreaId})
        end).

find_obj(Key) when is_reference(Key) ->
    case libstd_srv:get_obj(Key) of
            {ok, Obj} ->
                Obj;
            {error, _Reason} ->
                case get_obj(Key) of
                    {ok, Obj} ->
                        Obj;
                    {error, _Reason} ->
                        {error, no_obj}
                end
    end.
                
next_obj() ->
    AreaId = mnesia:dirty_first(shared_obj_registry),
    lookup_obj(AreaId).

next_obj(AreaId) ->
    NextAreaId = mnesia:dirty_next(shared_obj_registry, AreaId),
    lookup_obj(NextAreaId).

get_obj(AreaId) ->
    case lookup_obj(AreaId) of
        {error, no_obj} ->
            lookup_neighbours(AreaId);
        {ok, _Obj} = Obj ->
            Obj
    end.

lookup_obj(AreaId) ->
    case AreaId of
        '$end_of_table' ->
            {error, no_obj};
        _Key ->
            case mnesia:dirty_read({shared_obj_registry, AreaId}) of
                [Record] -> 
                    {ok, {Record#shared_obj_registry.ref, 
                        Record#shared_obj_registry.pid}};
                [] ->
                    {error, no_obj}
            end
    end.

lookup_neighbours(AreaId) ->
    lookup_neighbours(AreaId, libstd_srv:neighbours()).

lookup_neighbours(_AreaId, []) ->
    {error, no_obj};

lookup_neighbours(AreaId, [_Node | _Neighbours]) ->
    %case lookup_obj(AreaId, Node) of
    %    {error, no_obj} ->
    %        lookup_neighbours(AreaId, Neighbours);
    %    {ok, _Obj} = Obj ->
    %        Obj
    %end.    
    lookup_neighbours(AreaId, []).


