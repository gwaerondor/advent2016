-module(part2).
-export([run/1]).

run(Door_ID) ->
    Unsorted = find_key(Door_ID, 0, []),
    Sorted = lists:keysort(1, Unsorted),
    [ V || {_, V} <- Sorted ].

find_key(_, _, Found) when length(Found) >= 8 ->
    Found;
find_key(Door_ID, Index, Found) ->
    Indexed = Door_ID ++ integer_to_list(Index),
    case hash(Indexed) of
	[$0, $0, $0, $0, $0, Pos, Code | _] ->
	    Number_pos = position_number(Pos),
	    find_key(Door_ID, Index + 1, update_found(Found, Number_pos, Code));
	_ ->
	    find_key(Door_ID, Index + 1, Found)
    end.

hash(Door_ID) ->
    Bin = list_to_binary(Door_ID),
    Encrypted = crypto:hash(md5, Bin),
    bin_to_hex(Encrypted).

position_number(P) ->
    case P >= $a of
	true ->
	    9000;
	false ->
	    list_to_integer([P])
    end.	    

update_found(Found, Pos, Code) when Pos < 8 ->
    case proplists:get_value(Pos, Found) of
	undefined ->
	    [{Pos, Code} | Found];
	_ ->
	    Found
    end;
update_found(Found, _, _) ->
    Found.

%% bin_to_hex credit: stack overflow user jmuc from this thread:
%% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex

bin_to_hex(Bin) ->
    binary_to_list(<< <<(hex(H)), (hex(L))>> || <<H:4, L:4>> <= Bin >>).

hex(C) when C < 10 ->
    $0 + C;
hex(C) ->
    $a + C - 10.
