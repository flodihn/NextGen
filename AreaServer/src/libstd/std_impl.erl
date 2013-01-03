%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the standard implementation module for the standard library 
%% 'libstd'.
%% The module provides functions for creating an area, generating unique
%% ids, upgrading modules, registering and lookups of objects.
%% @end
%%----------------------------------------------------------------------
-module(libstd.std_impl).

-import(application).
-import(error_logger).
-import(mnesia).
-import(timer).
-import(lists).
-import(code).
-import(rpc).
-import(ets).
-import(re).

-import(shared_cache).

-include("obj_registry.hrl").
-include("shared_obj_registry.hrl").
-include("area_registry.hrl").

% API
-export([
    init/0,
    create_area/0,
    register_obj/2,
    unregister_obj/1,
    upgrade/1,
    get_obj/1,
    next_obj/0,
    next_obj/1,
    area_name/0,
    area_name/1,
    area_name/2,
    monsrv_rpc/2,
    monsrv_rpc/3,
    get_billboard/1,
    get_pid/1,
    get_all_pids/0
    ]).

% handlers
-export([
    area_event/1
    ]).

% internal exports
-export([
    ]).

%%----------------------------------------------------------------------
%% @spec upgrade(Module) -> {module, Module}
%% where
%%      Module = atom()
%% @doc
%% This function makes a hot code upgrade of the module Module.
%% @end
%%----------------------------------------------------------------------
upgrade(Module) ->
    code:purge(Module),
    code:load_file(Module).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @doc
%% This function initiates the standard library.
%% @end
%%----------------------------------------------------------------------
init() ->
    mnesia:start(),
    shared_cache:init(),
    mnesia:wait_for_tables([obj_registry], 1000),
    % Maybe make a better test for area has been created or joined before
    % registering in the monitor server.
    case lists:member(obj_registry, ets:all()) of
        true ->
            monsrv_rpc(monsrv, register_node, [node()]);
        false ->
            pass
    end.

%%----------------------------------------------------------------------
%% @spec create_area() -> ok
%% @doc
%% This function creates the area. The area name will be the same as
%% the Erlang node name. It should only be run once in the cluster.
%% If more computers are joined to the area the join_area/0 function
%% is used. 
%% @end
%%----------------------------------------------------------------------
create_area() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(obj_registry,
                [{ram_copies, [node()]},
                {local_content, true},
                {attributes, record_info(fields, obj_registry)}]),
    libdist.srv:create_area(),
    libenv.srv:create_area(),
    libsave.srv:create_area(),
    monsrv_rpc(monsrv, register_node, [node()]),
    ok.

%%----------------------------------------------------------------------
%% @spec area_name() -> AreaName
%% where
%%      AreaName = atom()
%% @doc
%% Returns the name of the area.
%% @end
%%----------------------------------------------------------------------
area_name() ->
    area_name(node(), []).

%%----------------------------------------------------------------------
%% @spec area_name(Node) -> AreaName
%% where
%%      Node = atom(),
%%      AreaName = atom()
%% @doc
%% Extracts the area name from the node Node. For example 
%% underworld@machine1 would return underworld.
%% @end
%%----------------------------------------------------------------------
area_name(Node) when is_atom(Node) ->
    area_name(Node, []);

area_name(Extra) when is_list(Extra) ->
    area_name(node(), Extra).

%%----------------------------------------------------------------------
%% @spec area_name(Node, Extra) -> AreaName
%% where
%%      Node = atom(),
%%      Extra = string(),
%%      AreaName = atom()
%% @doc
%% Same as area_name/1 but appends the string Extra do the area name.
%% @end
%%----------------------------------------------------------------------
area_name(Node, Extra) ->
    [NodeName, _Host] = re:split(atom_to_list(Node), "@", 
        [{return, list}]),
    list_to_atom(NodeName ++ Extra).

get_obj(Id) ->
	lookup_obj(Id).

next_obj() ->
	Key = mnesia:dirty_first(obj_registry),
	lookup_obj(Key).

next_obj(Id) ->
	NextKey = mnesia:dirty_next(obj_registry, Id),
	lookup_obj(NextKey).

lookup_obj(Id) ->
	case Id of
		'$end_of_table' ->
			{error, no_obj}; 
		_Key ->
            case mnesia:dirty_read({obj_registry, Id}) of
                [#obj_registry{ref=NextId, pid=Pid}] ->
                    {ok, NextId, Pid};
                [] ->
                    {error, no_obj}
            end
	end.

register_obj(Id, Pid) ->
    mnesia:dirty_write(#obj_registry{ref=Id, pid=Pid}),
    % Why does libstd messes with libdist db, this functionality should
    % exist in the libdist.std_impl.
    %mnesia:dirty_write(#shared_obj_registry{ref=Id, pid=Pid}),
    {ok, Id}.
 
unregister_obj(Id) ->
    mnesia:dirty_delete({obj_registry, Id}).

%%----------------------------------------------------------------------
%% @spec area_event(Event) -> ok 
%% where
%%      Event= any()
%% @doc
%% Sends the message Event to every object in the area.
%% @end
%%----------------------------------------------------------------------
area_event(Event) ->
    area_event(Event, mnesia:dirty_first(shared_obj_registry)),
    ok.

%%----------------------------------------------------------------------
%% @private
%% @spec area_event(Event, Key) -> ok 
%% where
%%      Event= any()
%% @doc
%% Iterates over the object registry sending the message to Event to
%% all objects until '$end_of_table' is reached.
%% @end
%%----------------------------------------------------------------------
area_event(_Event, '$end_of_table') ->
    ok;

area_event(Event, Key) ->
    [Reg] = mnesia:dirty_read(shared_obj_registry, Key),
    Reg#shared_obj_registry.pid ! Event,
    area_event(Event, mnesia:dirty_next(shared_obj_registry, Key)).

%%----------------------------------------------------------------------
%% @spec monsrv_rpc(Mod, Fun) -> any()
%% where
%%      Mod = atom(),
%%      Fun = atom()
%% @doc
%% Same as monsrv_rpc/3 with an empy list as argument Args.
%% @end
%%----------------------------------------------------------------------
monsrv_rpc(Mod, Fun) ->
    monsrv_rpc(Mod, Fun, []).

%%----------------------------------------------------------------------
%% @spec monsrv_rpc(Mod, Fun, Args) -> any()
%% where
%%      Mod = atom(),
%%      Fun = atom(),
%%      Args = list()
%% @doc
%% Makes a remote call in the monitor servers, calls the module Mod and
%% function Fun with the arguments Args. If more than one monitor server
%% exists in the cluster the round robin algoritm decides what server
%% to call.
%% @end
%%----------------------------------------------------------------------
monsrv_rpc(Mod, Fun, Args) ->
    {ok, MonSrvList} = application:get_env(areasrv, monsrv),
    monsrv_rpc(Mod, Fun, Args, MonSrvList).

%%----------------------------------------------------------------------
%% @private
%% @spec monsrv_rpc(Mod, Fun, Args, MonSrvList) -> any()
%% where
%%      Mod = atom(),
%%      Fun = atom(),
%%      Args = list(),
%%      MonSrvList = list()
%% @doc
%% Called by monsrv_rpc/3 and applies the round robin algoritm to 
%% decide what monitor server to call. The actual round robin 
%% algoritm is computed in the monitor server.
%% @end
%%----------------------------------------------------------------------
monsrv_rpc(_Mod, _Fun, _Args, []) ->
    {error, rpc_failed};

monsrv_rpc(Mod, Fun, Args, [MonNode | NodeList]) ->
    case rpc:call(MonNode, Mod, Fun, Args) of
        {badrpc, nodedown} ->
            monsrv_rpc(Mod, Fun, Args, NodeList);
        Result ->
            Result
    end.

%%----------------------------------------------------------------------
%% @deprecated
%% @spec get_billboard(Id) -> Billboard
%% where
%%      Id = id(),
%%      Billboard = string()
%% @doc
%% This function will be replaced get_property/3 in obj module.
%% @end
%%----------------------------------------------------------------------
get_billboard(Id) ->
    case get_obj(Id) of
        {error, no_obj} ->
            "Abydos/NoMesh";
        {ok, Record} ->
            Pid = Record#obj_registry.pid,
            Pid ! {self(), get_billboard},
            receive 
                {billboard, Billboard} ->
                   Billboard 
            after 1000 ->
                    timeout
            end
    end.

%%----------------------------------------------------------------------
%% @spec get_pid(Id) -> {ok, Pid} | {error, Reason}
%% where
%%      Id = id(),
%%      Pid = pid(),
%%      Reason = any()
%% @doc
%% Returns the process id of the object with the id Id.
%% @end
%% @depricated this is same as get_obj(Id). This fun should be removed.
%%----------------------------------------------------------------------
get_pid(Id) ->
    case get_obj(Id) of
        {error, no_obj} ->
            {error, no_obj};
        {ok, Id, Pid} ->
            {ok, Pid}
    end.
            
get_all_pids() ->
    get_all_pids(next_obj(), []).

get_all_pids({error, no_obj}, Acc) ->
    Acc;

get_all_pids({ok, Id, Pid}, Acc) ->
    get_all_pids(next_obj(Id), Acc ++ [Pid]).      



