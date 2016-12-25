-module(day20).
-export([run/0, run/1]).

run() ->
    run("input.txt").

run(Input) ->
    Intervals = parse_intervals(Input),
    find_first_unincluded(Intervals).

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

do_find([], Initial, Current, Initial_lowest) ->
    io:format("Finished one loop. Started: ~p, now: ~p~n", [Initial_lowest, Current]),
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
    
