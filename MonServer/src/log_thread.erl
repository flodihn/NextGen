-module(log_thread).

-export([
    init/1,
    log_thread/1
    ]).

init(Interval) ->
    Pid = spawn_link(?MODULE, log_thread, [Interval]),
    register(?MODULE, Pid).

log_thread(Interval) ->
    receive 
        stop ->
            unregister(?MODULE),
            exit;
        _ ->
            log_thread(Interval)
    after Interval ->
        monsrv:log(),
        log_thread(Interval)
    end.

