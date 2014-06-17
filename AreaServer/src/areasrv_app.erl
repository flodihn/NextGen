-module(areasrv_app).
-behaviour(application).

-export([
    start/0,
    start/1,
    start/2,
    stop/1,
    auto_load_libs/0
    ]).


start() ->
	start([], []).

start([]) ->
	start([], []).

start(_Type, StartArgs) ->
    Result = areasrv_sup:start_link(StartArgs),
    spawn(?MODULE, auto_load_libs, []),
    Result.

stop(_State) ->
    ok.

auto_load_libs() ->
	Result = application:get_env(areasrv, libs_auto_load),
	case Result of
		{ok, Libs} ->
			load_libs(Libs);
		Error ->
			error_logger:info_report([{?MODULE, warning,
				no_auto_loadings_libs_in_app_file, Error}])
	end.

load_libs([]) ->
    done;

load_libs([Lib|Tail]) ->
    Result = lib_sup:start(Lib),
    error_logger:info_report([{auto_loading_library, Lib, result, Result}]),
    load_libs(Tail).
