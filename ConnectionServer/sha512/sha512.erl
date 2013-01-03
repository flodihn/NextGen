-module(sha512).
-export([start/0, stop/0, init/1]).
-export([hexdigest/1]).

start() ->
    case erl_ddll:load_driver(".", "sha512_drv") of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end,
    spawn(?MODULE, init, ["sha512_drv"]).

init(SharedLib) ->
    register(sha512, self()),
    Port = open_port({spawn, SharedLib}, []),
    loop(Port).

stop() ->
    sha512 ! stop.

hexdigest(Text) ->
    sha512 ! {call, self(), Text},
    receive
        {sha512, Result} ->
            hex_to_string(Result)
    end.

loop(Port) ->
    receive
        {call, Caller, gaylord} ->
            io:format("GYALORRD"),
            loop(Port);
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {sha512, decode(Data)}
            end,
            loop(Port);

        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                exit(normal)
            end;

        {'EXIT', Port, Reason} ->
            io:format("~p ~n", [Reason]),
            exit(port_terminated)
    end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y];
encode(Str) -> Str.

decode([Int]) -> Int;
decode(Str) -> Str.

hex_to_string(List) ->
    hex_to_string(List, []).

hex_to_string([Head | Tail], Acc) ->
    Char = io_lib:format("~2.16.0b", [Head]),
    hex_to_string(Tail, lists:append(Acc, Char));

hex_to_string([], Acc) ->
    lists:flatten(Acc).    

