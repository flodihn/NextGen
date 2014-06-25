-module(util).

-include("vec.hrl").

-export([
    make_vec/1,
    vector_add/2,
    vector_diff/2,
    vector_mult/2,
    rand_float/0,
    rand_int/1,
    rand_negative_int/1,
    normalize/1,
    floor/1,
    ceiling/1,
    trim_int/3
    ]).

vector_add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2};

vector_add({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 + X2, Y1 + Y2, Z1 + Z2};

vector_add({X1, Y1}, undefined) ->
    {X1, Y1};

vector_add(undefined, {X2, Y2}) ->
    {X2, Y2};

vector_add({X1, Y1, Z1}, undefined) ->
    {X1, Y1, Z1};

vector_add(undefined, {X2, Y2, Z2}) ->
    {X2, Y2, Z2};

vector_add(#vec{x=X1, y=Y1, z=Z1}, #vec{x=X2, y=Y2, z=Z2}) ->
    #vec{x=X1+X2, y=Y1+Y2, z=Z1+Z2};

vector_add(#vec{x=X1, y=Y1, z=Z1}, {X2, Y2, Z2}) ->
    #vec{x=X1+X2, y=Y1+Y2, z=Z1+Z2};

vector_add({x=X1, y=Y1, z=Z1}, #vec{x=X2, y=Y2, z=Z2}) ->
    #vec{x=X1+X2, y=Y1+Y2, z=Z1+Z2};

vector_add(undefined, #vec{} = Vec) ->
    Vec;

vector_add(#vec{} = Vec, undefined) ->
    Vec.

make_vec(#vec{} = Vec) ->
    Vec;

make_vec({X, Y, Z}) ->
    #vec{x=X, y=Y, z=Z}.

vector_diff(#vec{x=X1, y=Y1, z=Z1}, #vec{x=X2, y=Y2, z=Z2}) ->
    DiffX = abs(X1) - abs(X2), 
    DiffY = abs(Y1) - abs(Y2), 
    DiffZ = abs(Z1) - abs(Z2),
    round(abs(DiffX + DiffY + DiffZ)).

vector_mult(#vec{x=X, y=Y, z=Z}, Multi) when is_number(Multi) ->
    #vec{x=X * Multi, y=Y * Multi, z=Z * Multi}.

rand_float() ->
    create_seed(),
    %random:seed(now()),
    case random:uniform(2) of
        1 ->
            %random:seed(now()),
            random:uniform();
        2 ->
            %random:seed(now()),
            -random:uniform()
    end.

rand_int(Nr) ->
    create_seed(),
    random:uniform(Nr).

rand_negative_int(Nr) ->
    create_seed(),
    case random:uniform(2) of
        1 ->
            random:uniform(Nr);
        2 ->
            -random:uniform(Nr)
    end.

create_seed() ->
    case get(random_seed) of
        undefined ->
            random:seed(now());
        _Seed ->
            Rand = random:uniform(9999999),
            {_T1, T2, T3} = now(), 
            random:seed({Rand, T2, T3})
            
    end.

normalize(#vec{x=X, y=Y, z=Z} = Vec) ->
    L = math:sqrt((X*X) + (Y*Y) + (Z*Z)),
	case L of 
		0.0 -> Vec;
    	_NonZero -> #vec{x=X/L, y=Y/L, z=Z/L}
	end.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(0.0) ->
    1;
ceiling(0) ->
    1;
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

trim_int(Lowest, _Highest, Nr) when Nr < Lowest ->
    Lowest;

trim_int(_Lowest, Highest, Nr) when Nr > Highest ->
    Highest;

trim_int(_Lowest, _Highest, Nr) ->
    Nr.
