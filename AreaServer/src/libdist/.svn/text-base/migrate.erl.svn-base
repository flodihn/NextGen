-module(libdist.migrate).

-include("obj.hrl").

-import(error_logger).
-import(rpc).

-define(MAX_MESSAGES, 1000).

% API
-export([
    init/0,
    migrate/2
    ]).

% handlers
-export([
    ]).

% internal exports
-export([
    migrate_loop/4
    ]).

init() ->
	ok.

migrate(AreaSrv, ObjState) ->
	Reply = rpc:call(AreaSrv, obj_sup, start, [ObjState#obj.type, 
        ObjState]),
	case Reply of
		{ok, NewPid} ->
			?MODULE:migrate_loop(AreaSrv, NewPid, 0, ObjState);
		{error, Reason} ->
			{migration_failed, Reason}
	end.

% Migrated objects enter this loop to die gracefully.
migrate_loop(AreaSrv, NewPid, NrMsg, #obj{id=Id} = State) ->
	receive 
		Event ->
			NewPid ! Event,
			?MODULE:migrate_loop(AreaSrv, NewPid, NrMsg + 1, State)
	after 1000 ->
        % If we migrated to a new area, unregister from the shared obj 
        % registry.
        case libstd.srv:area_name() == libstd.srv:area_name(AreaSrv) of
            true ->
                ok;
            false ->
                libdist.srv:unregister_obj(Id)
        end,
		libstd.srv:unregister_obj(Id),
		error_logger:info_report([{migration_successful, Id}]),
        exit(normal)
	end;

% Force shutdow after 1000 relayed messages
migrate_loop(_AreaSrv, _NewPid, ?MAX_MESSAGES, _State) ->
    error_logger:info_report([{migration_forced, 
        {max_messages, ?MAX_MESSAGES}}]),
    exit(normal).

