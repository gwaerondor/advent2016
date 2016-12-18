-module(part1).
-export([run/1]).

run(Door_ID) ->
    find_key(Door_ID, 0, []).

find_key(_, _, Found) when length(Found) >= 8 ->
    Found;
find_key(Door_ID, Index, Found) ->
    Indexed = Door_ID ++ integer_to_list(Index),
    case hash(Indexed) of
	[$0, $0, $0, $0, $0, X | _] ->
	    find_key(Door_ID, Index + 1, Found ++ [X]);
	_ ->
	    find_key(Door_ID, Index + 1, Found)
    end.

hash(Door_ID) ->
    Bin = list_to_binary(Door_ID),
    Encrypted = crypto:hash(md5, Bin),
    bin_to_hex(Encrypted).

%% bin_to_hex credit: stack overflow user jmuc from this thread:
%% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex

bin_to_hex(Bin) ->
    binary_to_list(<< <<(hex(H)), (hex(L))>> || <<H:4, L:4>> <= Bin >>).

hex(C) when C < 10 ->
    $0 + C;
hex(C) ->
    $a + C - 10.

