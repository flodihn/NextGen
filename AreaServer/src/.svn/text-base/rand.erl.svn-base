-module(rand).

-export([
    int/2,
    test/0
    ]).

init() ->
    crypto:start().

int(Min, Max) ->
    init(),
    crypto:rand_uniform(Min, Max).

test() ->
    test(100).

test(0) ->
    done;

test(Nr) ->
    Rand = int(0, 100),
    io:format("~p.~n", [Rand]),
    test(Nr - 1).

