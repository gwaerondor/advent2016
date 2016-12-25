-module(day15).
-export([run/1]).
-record(disc, {current, positions}).

run(File) ->
    State = parse_disc_states(File),
    Extra_spec = "Disc #7 has 11 positions; at time=0, it is at position 0.",
    Extra_disc = make_state(Extra_spec),
    New_state = State ++ [Extra_disc],
    First_drop_time = find_drop_time_from(0, New_state, 7),
    io:format("For part 1, drop at: ~p~n", [First_drop_time]),
    Second_drop_time = find_drop_time_from(First_drop_time, New_state, 8),
    io:format("For part 2, drop at: ~p~n", [Second_drop_time]).
    
positions_at_time(T, State) ->
    [ {I, tick(Disc, T)} || {I, Disc} <- State ].

tick(Disc, Steps) ->
    Curr = Disc#disc.current,
    Positions = Disc#disc.positions,
    Next = (Curr + Steps) rem Positions,
    Disc#disc{current = Next}.

parse_disc_states(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Tokens = string:tokens(String, "\n"),
    [ make_state(L) || L <- Tokens ].

make_state(L) ->
    RE = "Disc #(\\d+) has (\\d+) positions;.*position (\\d+).",
    {match, M} = re:run(L, RE, [{capture, all, list}]),
    Number = list_to_integer(lists:nth(2, M)),
    Curr = list_to_integer(lists:nth(4, M)),
    Positions = list_to_integer(lists:nth(3, M)),
    {Number, #disc{current = Curr,
		   positions = Positions}}.

find_drop_time_from(Time, State, Target) ->
    drop(State, Time, Time, 1, Target).

drop(_, Initial_time, _, Target, Target) ->
    Initial_time-1;
drop(State, Initial_time, Current_time, Number, Target) ->
    New_state = positions_at_time(Current_time, State),
    Disc = get_disc(Number, New_state),
    case Disc#disc.current of
	0 ->
	    drop(State, Initial_time, Current_time + 1, Number + 1, Target);
	_ ->
	    drop(State, Initial_time + 1, Initial_time + 1, 1, Target)
    end.

get_disc(Number, State) ->
    proplists:get_value(Number, State).
