-module(quad).

-include("vec.hrl").

% API
-export([
    query_area/1,
    calc_new_pos/2
    ]).

% handlers
-export([
    ]).

query_area(ObjPos) ->
    {SizeX, SizeY} = geo_funs:get_size(),
    BorderEast = SizeX - (SizeX/2),
    BorderWest = -(SizeX - (SizeX/2)),
    BorderNorth = SizeY - (SizeY/2),
    BorderSouth = -(SizeY - (SizeY/2)),
    
    query_border(ObjPos, BorderEast, BorderWest, BorderNorth,
        BorderSouth).

query_border(ObjPos, BorderEast, _BorderWest, BorderNorth, 
    _BorderSouth) when
    ObjPos#vec.z > BorderNorth andalso ObjPos#vec.x > BorderEast ->
    geo_funs:get_neighbour(southeast); 

query_border(ObjPos, _BorderEast, BorderWest, BorderNorth, 
    _BorderSouth) when
    ObjPos#vec.z > BorderNorth andalso ObjPos#vec.x < BorderWest->
    geo_funs:get_neighbour(southwest); 

query_border(ObjPos, BorderEast, _BorderWest, _BorderNorth, 
    BorderSouth) when
    ObjPos#vec.z < BorderSouth andalso ObjPos#vec.x > BorderEast->
    geo_funs:get_neighbour(northeast); 

query_border(ObjPos, _BorderEast, BorderWest, _BorderNorth, 
    BorderSouth) when
    ObjPos#vec.z < BorderSouth andalso ObjPos#vec.x < BorderWest->
    geo_funs:get_neighbour(northwest); 

query_border(ObjPos, _BorderEast, _BorderWest, BorderNorth, 
    _BorderSouth) when
    ObjPos#vec.z > BorderNorth ->
    {ok, Area} = geo_funs:get_neighbour(south),
    %NewPos = calc_new_pos(Area, ObjPos, south),
    {Area, south};

query_border(ObjPos, _BorderEast, _BorderWest, _BorderNorth, 
    BorderSouth) when
    ObjPos#vec.z < BorderSouth->
    {ok, Area} = geo_funs:get_neighbour(north),
    %NewPos = calc_new_pos(Area, ObjPos, north),
    {Area, north};

query_border(ObjPos, BorderEast, _BorderWest, _BorderNorth, 
    _BorderSouth) when
    ObjPos#vec.x > BorderEast->
    geo_funs:get_neighbour(east); 

query_border(ObjPos, _BorderEast, BorderWest, _BorderNorth, 
    _BorderSouth) when
    ObjPos#vec.x < BorderWest->
    geo_funs:get_neighbour(west); 

query_border(ObjPos, _BorderEast, _BorderWest, _BorderNorth,
    _BorderSouth) ->
    {libstd:area_name(), ObjPos}.

calc_new_pos(Pos, Border) ->
    %Node = std_funs:monsrv_rpc(mon, get_area, [Area]),
    %{X, Y} = rpc:call(Node, libgeo, get_pos, []),
    %AreaPos = #vec{x=X, y=Y},
    case Border of
        north ->
            Pos#vec{z=(Pos#vec.z + 1)  * -1};
        south ->
            Pos#vec{z=(Pos#vec.z - 1)* -1};
        Other ->
            error_logger:info_report([{?MODULE, calc_new_pos, error, Other}])
    end.


