-module(abydos_protocol).

-include("vec.hrl").
-include("protocol_mixed_types.hrl").

% States
-export([
    make_str/1,
    make_vec/1,
    keyval_list_to_binary/1,
    make_mixed_list/1
    ]).

make_str(String) when is_binary(String) ->
    Len = byte_size(String),
    <<Len, String/binary>>;

make_str(String) when is_list(String) ->
    make_str(list_to_binary(String));

make_str(Term) when is_atom(Term) ->
    BinString = list_to_binary(atom_to_list(Term)),
    StrLen = byte_size(BinString),
    <<StrLen/integer, BinString/binary>>;

make_str(_String) ->
    Str = list_to_binary("null"),
    StrLen = byte_size(Str),
    <<StrLen/integer, Str/binary>>.

make_vec(#vec{x=X, y=Y, z=Z}) ->
    <<X/little-float, Y/little-float, Z/little-float>>.

make_mixed(Term) when is_atom(Term) ->
    Mixed = make_str(Term),
    <<?MIXED_STRING, Mixed/binary>>;

make_mixed(Term) when is_integer(Term) ->
    <<?MIXED_INTEGER, Term/integer>>;

make_mixed(Term) when is_float(Term) ->
    <<?MIXED_FLOAT, Term/little-float>>;

make_mixed(Term) when is_list(Term) ->
    Mixed = make_str(Term),
    <<?MIXED_STRING, Mixed/binary>>;

make_mixed(#vec{x=X, y=Y, z=Z}) ->
    <<?MIXED_VECTOR, X/little-float, Y/little-float, 
        Z/little-float>>;

make_mixed(Term) ->
    BinStr = make_str(Term),
    <<?MIXED_STRING, BinStr/binary>>.

make_mixed_list(List) ->
    ListLen = length(List),
    make_mixed_list(List, <<ListLen/integer>>).

make_mixed_list([], Acc) ->
    Acc;

make_mixed_list([Head | Tail], Acc) ->
    Mixed = make_mixed(Head), 
    make_mixed_list(Tail, <<Acc/binary, Mixed/binary>>).

keyval_list_to_binary(List) ->
    Len = length(List),
    keyval_list_to_binary(List, <<Len>>).

keyval_list_to_binary([], Acc) ->
    Acc;

keyval_list_to_binary([{Key, Val} | Rest], Acc) ->
    BinKey = make_mixed(Key),
    BinVal = make_mixed(Val),
    %error_logger:info_report({keyval_list_to_binary, Key, BinKey,
    %    Val}),
    keyval_list_to_binary(Rest, <<Acc/binary, BinKey/binary, 
        BinVal/binary>>).

     
