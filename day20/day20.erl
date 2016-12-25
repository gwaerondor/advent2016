-module(day20).
-export([run/0, run/1]).

run() ->
    run("input.txt").

run(Input) ->
    Intervals = parse_intervals(Input),
    Unincluded = find_first_unincluded(Intervals),
    Amount = find_amount_of_unincluded(Intervals),
    io:format("First available IP: ~p~n"
	      "Number of available IPs: ~p~n",
	      [Unincluded, Amount]).

parse_intervals(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Tokens = string:tokens(String, "\n"),
    [ make_interval(T) || T <- Tokens ].

make_interval(Text) ->
    [Lower, Upper] = string:tokens(Text, "-"),
    {list_to_integer(Lower), list_to_integer(Upper)}.

find_first_unincluded(Intervals) ->
    do_find(Intervals, Intervals, 0, 0).

find_amount_of_unincluded(Intervals) ->
    do_find_2(Intervals, Intervals, 0, 0, 0).

do_find([], Initial, Current, Initial_lowest) ->
    case Current == Initial_lowest of
	true ->
	    Current;
	false ->
	    do_find(Initial, Initial, Current, Current)
    end;
do_find([{L, U} | R], Initial_intervals, Current, Initial_lowest) ->
    case L =< Current andalso U >= Current of
	true ->
	    do_find(R, Initial_intervals, U+1, Initial_lowest);
	false ->
	    do_find(R, Initial_intervals, Current, Initial_lowest)
    end.

do_find_2(_, _, 4294967296, _, Amount) ->
    Amount;    
do_find_2([], Initial, Current, Initial_lowest, Amount) ->
    case Current == Initial_lowest of
	true ->
	    do_find_2(Initial, Initial, Current+1, Current+1, Amount+1);
	false ->
	    do_find_2(Initial, Initial, Current, Current, Amount)
    end;
do_find_2([{L, U} | R], Initial_intervals, Current, Initial_lowest, Amount) ->
    case L =< Current andalso U >= Current of
	true ->
	    do_find_2(R, Initial_intervals, U+1, Initial_lowest, Amount);
	false ->
	    do_find_2(R, Initial_intervals, Current, Initial_lowest, Amount)
    end.
    
