-module(day19).
-export([run/0, run_part_1/1, run_part_2_slow/1]).
-define(ELVES, 3012210).
run() ->
    Part_1 = run_part_1(?ELVES),
    io:format("Part 1: ~p~n", [Part_1]),
    Part_2 = run_part_2(?ELVES),
    io:format("Part 2: ~p~n", [Part_2]).

%%% --------------------------------------------------------

run_part_1(Number) ->
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

%%% --------------------------------------------------------
%%% The following solution to part 2, while valid,
%%% takes days to run. I'm leaving it in for posterity.

run_part_2_slow(Number) ->
   Separator = trunc(Number/2),
   Low_elves = lists:seq(1, Separator),
   High_elves = lists:seq(Separator + 1, Number),
   execute_2(Low_elves ++ High_elves).

execute_2([Elf]) ->
    Elf;
execute_2([Elf|Elves]) ->
    io:format("~p~n", [[Elf|Elves]]),
    Remove_index = trunc((1+length(Elves)) / 2),
    Removed = remove_from_index(Remove_index, Elves),
    New_elves = lists:reverse([Elf|lists:reverse(Removed)]),
    execute_2(New_elves).

remove_from_index(Index, List) ->
    {Keep_left, [_|Keep_right]} = lists:split(Index - 1, List),
    lists:append(Keep_left, Keep_right).

%%% ----------------------------
%%% The following solution to part 2 is blatantly stolen from
%%% github user joskov, this page:
%%% https://github.com/joskov/advent2016/blob/master/day19/task2.ex

run_part_2(Number) ->
    calculate(Number).

simulation(_, Index, Limit, Limit) ->
    {Limit, Limit - Index};
simulation(List, Index, Size, Limit) ->
    Add_index = modulo(Index + divide(Size + 1, 2) - 1, Size) + 1,
    New_size = Size + 1,
    New_index =
	case (Index > Add_index) of
	    true ->
		Index;
	    false ->
		modulo(New_size + Index - 1, New_size)
	end,
    simulation(List, New_index, New_size, Limit).

calculate(Input) ->
    simulation([1], 0, 1, Input).

modulo(A, B) ->
    A rem B.

divide(A, B) ->
    A div B.
