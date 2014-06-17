%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the standard implementation for the id library 'libid'.
%% The module provides functionality to generate new unique ids.
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(libid_std_impl).

-include("char.hrl").

-export([
    init/0,
    generate_id/0
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the id library.
%% @end
%%----------------------------------------------------------------------
init() ->
    ok.

%%----------------------------------------------------------------------
%% @spec generate_id() -> {ok, Id}
%% where
%%      Id = id()
%% @doc
%% Generates a unique id.
%% @end
%%----------------------------------------------------------------------
generate_id() ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    {ok, list_to_binary(
        atom_to_list(node()) ++ "#" ++
        integer_to_list(MegaSec) ++ 
        integer_to_list(Sec) ++ 
        integer_to_list(MicroSec))}.
