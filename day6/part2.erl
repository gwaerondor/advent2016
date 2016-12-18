-module(part2).
-export([run/1]).
-define(SIGNAL_LENGTH, 8).

run(File) ->
    Signals = get_signals(File),
    By_index = arrange_by_index(Signals),
    [ most_common(L) || L <- By_index ].

get_signals(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    string:tokens(String, "\n").

arrange_by_index(Signals) ->
    [get_all_at_pos(N, Signals) || N <- lists:seq(1, ?SIGNAL_LENGTH) ].

get_all_at_pos(N, Signals) ->
    [ lists:nth(N, Signal) || Signal <- Signals ].

most_common(L) ->
    C = count(L, []),
    S = lists:keysort(2, C),
    {Res, _} = hd(S),
    Res.

count([], Acc) ->
    Acc;
count([H | T], Acc) ->
    count(T, increment(H, Acc)).

increment(Letter, Acc) ->
    case proplists:get_value(Letter, Acc) of
	undefined ->
	    [{Letter, 1} | Acc];
	N ->
	    lists:keyreplace(Letter, 1, Acc, {Letter, N + 1})
    end.
