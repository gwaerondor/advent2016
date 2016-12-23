-module(part1).
-export([run/1]).
-record(disc, {number, current, positions}).

run(File) ->
    State = parse_disc_states(File),
    find_drop_time(State).
    
positions_at_time(0, State) ->
    State;
positions_at_time(T, State) ->
    New_state = [ tick(Disc) || Disc <- State ],
    positions_at_time(T-1, New_state).

tick(Disc) ->
    Curr = Disc#disc.current,
    Positions = Disc#disc.positions,
    Next = (Curr + 1) rem Positions,
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
    #disc{number = Number,
	  current = Curr,
	  positions = Positions}.

find_drop_time(State) ->
    drop(State, 0, 0, 1).

drop(State, Initial_time, _, 7) ->
    io:format("Reached at t0 = ~p~n"
	      "State is: ~p~n", [Initial_time-1, positions_at_time(Initial_time-1, State)]),
    Initial_time-1;
drop(State, Initial_time, Current_time, Number) ->
    New_state = positions_at_time(Current_time, State),
    Disc = get_disc(Number, New_state),
    case Disc#disc.current of
	0 ->
	    drop(State, Initial_time, Current_time + 1, Number + 1);
	_ ->
	    drop(State, Initial_time + 1, Initial_time + 1, 1)
    end.

get_disc(Number, [H|T]) ->
    case H#disc.number == Number of
	true ->
	    H;
	false ->
	    get_disc(Number, T)
    end.
