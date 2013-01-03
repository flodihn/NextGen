-module(geo_funs).

-include("geo_data.hrl").
-include("neighbour.hrl").

% API
-export([
    init/0,
    create_area/0,
    join/1,
    set_pos/1,
    get_pos/0,
    set_shape/1,
    set_size/1,
    get_size/0,
    query_area/1,
    set_neighbour/2,
    del_neighbour/1,
    get_neighbour/1,
    calc_new_pos/2
    ]).

% handlers
-export([
    ]).

init() ->
    ok.
 
% Only call once per area.
create_area() ->
    mnesia:create_table(geo_data, 
        [{disc_copies, [node()]},
        {attributes, record_info(fields, geo_data)}]),
    mnesia:create_table(neighbour, 
        [{disc_copies, [node()]},
        {attributes, record_info(fields, neighbour)}]).    
   
join(Node) ->
    rpc:call(Node, mnesia, add_table_copy, 
        [geo_data, node(), disc_copies]),
    rpc:call(Node, mnesia, add_table_copy, 
        [neighbour, node(), disc_copies]).

set_pos(Pos) ->
    F = fun() ->
        mnesia:write(#geo_data{key=pos, value=Pos})
    end,
    mnesia:transaction(F).

get_pos() ->
    case mnesia:dirty_read({geo_data, pos}) of
        [Rec] ->
            Rec#geo_data.value;
        [] ->
            {0, 0}
    end.


set_size(Size) ->
    F = fun() ->
        mnesia:write(#geo_data{key=size, value=Size})
    end,
    mnesia:transaction(F).

get_size() ->
    case mnesia:dirty_read({geo_data, size}) of
        [Rec] ->
            Rec#geo_data.value;
        [] ->
            {0, 0}
    end.

set_shape(Shape) ->
    case code:which(Shape) of
        non_existing ->
            {error, unsupported_shape};
        _ ->
            F = fun() ->
                mnesia:write(#geo_data{key=shape, value=Shape})
            end,
            mnesia:transaction(F),
            {ok, shape_set}
    end.

query_area(ObjPos) ->
    case mnesia:dirty_match_object(#geo_data{key=shape, value='$1'}) of
	        [] ->
	            {error, area_shape_not_set};
	        [Rec] ->
                Shape = Rec#geo_data.value, 
	            case code:which(Shape) of
		            non_exsting ->
		                {error, unknown_shape};
		            _ ->
		                Shape:query_area(ObjPos)
	            end
    end.
           
set_neighbour(Slot, Area) ->
    F = fun() ->
        mnesia:write(#neighbour{slot=Slot, area=Area})
    end,
    mnesia:transaction(F).

del_neighbour(Slot) ->
    F = fun() ->
        mnesia:delete({neighbour, Slot})
    end,
    mnesia:transaction(F).

get_neighbour(Slot) ->
    case mnesia:dirty_match_object(#neighbour{slot=Slot, area='$1'}) of
	    [Rec] ->
	        {ok, Rec#neighbour.area};
	    [] ->
	        {ok, std_funs:area_name()}
    end.

calc_new_pos(Pos, Border) ->
    case mnesia:dirty_match_object(#geo_data{key=shape, value='$1'}) of
	        [] ->
	            {error, area_shape_not_set};
	        [Rec] ->
                Shape = Rec#geo_data.value, 
	            case code:which(Shape) of
		            non_exsting ->
		                {error, unknown_shape};
		            _ ->
		                Shape:calc_new_pos(Pos, Border)
	            end
    end.
 
