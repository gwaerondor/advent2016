-module(day19).
-export([run/0, run/1]).

run() ->
    run(3012210).

run(Number) ->
    Elves = lists:seq(1, Number),
    execute(Elves).

execute([Elf]) ->
    Elf;
execute(Elves) ->
    Reduced = remove_every_second(Elves),
    Even = length(Elves) rem 2 == 0,
    New = case Even of
	      true ->
		  Reduced;
	      false ->
		  place_last_first(Reduced)
	  end,
    execute(New).

remove_every_second([]) ->
    [];
remove_every_second([E]) ->
    [E];
remove_every_second([X, _ | R]) ->
    [X | remove_every_second(R)].

place_last_first(L) ->
    [Last | Rest] = lists:reverse(L),
    [Last | lists:reverse(Rest)].
