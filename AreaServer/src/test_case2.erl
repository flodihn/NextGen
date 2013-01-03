% This file contains code to run the load balancing test case
% in the "Next Generation MMO Architeture" article.
-module(test_case2).

-export([
    run/0
    ]).

run() ->
    Group1 = dev:create_robots(1000),
    Group2 = dev:cteate_robots(1000).



