-module(day14).
-export([run/1, hash_2017/1]).


run(Salt) ->
    put(pre, []),
    find(Salt, 0, []).

find(Salt, N, Found) when length(Found) < 64 ->
    V = Salt ++ integer_to_list(N),
    case find_threes(V) of
	none ->
	    find(Salt, N+1, Found);
	X ->
	    case next_1000_has_five(Salt, N, X) of
		true ->
		    find(Salt, N+1, [N | Found]);
		false ->
		    find(Salt, N+1, Found)
	    end
    end;
find(_, _, Found) -> hd(Found).


find_threes(V) ->
    Hash = hash_2017(V),
    find_three_in_a_row(Hash).

find_three_in_a_row([]) ->
    none;
find_three_in_a_row([X, X, X | _]) ->
    X;
find_three_in_a_row([_|T]) ->
    find_three_in_a_row(T).

next_1000_has_five(Salt, Index, To_find) ->
    next_1000_has_five(Salt, Index, Index + 1000, To_find).

next_1000_has_five(_, Index, Index, _) ->
    false;
next_1000_has_five(Salt, Index, Current, To_find) ->
    Hash = hash_2017(Salt ++ integer_to_list(Current)),
    case are_there_five_in_a_row(Hash, To_find) of
	true ->
	    true;
	false ->
	    next_1000_has_five(Salt, Index, Current - 1, To_find)
    end.

are_there_five_in_a_row([], _) ->
    false;
are_there_five_in_a_row([C,C,C,C,C|_], C) ->
    true;
are_there_five_in_a_row([_|T], C) ->
    are_there_five_in_a_row(T, C).
    

hash(Value) ->
    Bin = list_to_binary(Value),
    Encrypted = crypto:hash(md5, Bin),
    bin_to_hex(Encrypted).

hash_2017(Value) ->
    Pre_hashed = get(pre),
    case proplists:get_value(Value, Pre_hashed) of
	undefined ->
	    H = lists:foldl(fun(_, Acc) ->
				    hash(Acc)
			    end,
			    Value,
			    lists:seq(0,2016)),
	    put(pre, [{Value, H} | Pre_hashed]),
	    H;
	X ->
	    X
    end.

%% bin_to_hex credit: stack overflow user jmuc from this thread:
%% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex

bin_to_hex(Bin) ->
    binary_to_list(<< <<(hex(H)), (hex(L))>> || <<H:4, L:4>> <= Bin >>).

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.


