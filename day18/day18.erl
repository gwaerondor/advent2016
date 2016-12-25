-module(day18).
-export([run/0, run/1]).

run() ->
    {ok, Bin} = file:read_file("input.txt"),
    Input = binary_to_list(Bin) -- "\n",
    run(Input).
    
run(Input) ->
    First = [ to_tile(C) || C <- Input ],
    Final = lists:foldl(fun(_, Acc) ->
				Curr = lists:last(Acc),
				Next = get_next(Curr),
				Acc ++ [Next]
			end, [First], lists:seq(2, 40)),
    All_tiles = lists:flatten(Final),
    Safe_tiles = lists:foldl(fun(safe, Sum) ->
				     Sum + 1;
				(_, Sum) ->
				     Sum
			     end, 0, All_tiles),
    io:format("Amount of safe tiles: ~p~n", [Safe_tiles]).

to_tile($.) ->
    safe;
to_tile($^) ->
    trap.

to_string([]) ->
    [];
to_string([trap|R]) ->
    [$^ | to_string(R)];
to_string([safe|R]) ->
    [$. | to_string(R)].

get_next(L) ->
    [next(N, L) || N <- lists:seq(1, length(L))].

next(N, List) ->
    L = get_at(N-1, List),
    M = get_at(N, List),
    R = get_at(N+1, List),
    make_next_tile(L, M, R).

get_at(N, List) ->
    case N =< length(List) andalso 1 =< N of
	true ->
	    lists:nth(N, List);
	false ->
	    safe
    end.
    
make_next_tile(trap, trap, safe) -> trap;
make_next_tile(safe, trap, trap) -> trap;
make_next_tile(trap, safe, safe) -> trap;
make_next_tile(safe, safe, trap) -> trap;
make_next_tile(_, _, _) -> safe.
