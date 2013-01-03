-module(migrate).

-export([
	migrate/2
	]).

migrate(From, To) ->
	Obj = rpc:call(From, libstd, get_obj, []),
	migrate(From, To, Obj).

migrate(From, To, {Key, ObjPid}) ->
	ObjPid ! {migrate, To},
	Next = rpc:call(From, libstd, get_obj, [Key]),
	migrate(From, To, Next);

migrate(From, To, '$end_of_table') ->
	error_logger:info_report([{migrate_report, successful, from, From, 
		to, To}]).



