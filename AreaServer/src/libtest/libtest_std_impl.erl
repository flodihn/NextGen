%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(libtest_std_impl).

-export([
    init/0,
    start_profiling/0,
    stop_profiling/0
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the test library.
%% @end
%%----------------------------------------------------------------------
init() ->
    ok.

start_profiling() ->
    eprof:start(), 
    Libs = get_libs(),
    eprof:start_profiling(Libs).

stop_profiling() ->
    eprof:stop_profiling(),
    eprof:log("profile.log"),
    eprof:total_analyse(),
    eprof:stop().

get_libs() ->
    Libs = get_libs(supervisor:which_children(lib_sup), []),
    % Have to change this when obj_sup is moved into a library.
    Extra = [whereis(obj_sup), whereis(lib_sup)],
    Libs ++ Extra.

get_libs([], Acc) ->
    Acc;

get_libs([Head | Tail], Acc) ->
    {_LibName, Pid, _Type, _Modules} = Head,
    get_libs(Tail, Acc ++ [Pid]).

