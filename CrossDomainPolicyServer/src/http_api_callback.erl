-module(http_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, Args) ->
    handle(Req#req.method, elli_request:path(Req), Req, Args).

handle('GET',[<<"crossdomain.xml">>], Req, Args) ->
	{200, [], <<"<?xml version=\"1.0\"?>\n<cross-domain-policy>\n<allow-access-from domain=\"*\"/>\n</cross-domain-policy>">>};

handle(_, _, _Req, Args) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
