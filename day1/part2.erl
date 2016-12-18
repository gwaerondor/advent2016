-module(part2).
-export([run/1]).

run(File) ->
    Instructions = to_instructions(File),
    put(all_visited, []),
    calculate_position(Instructions, north, {0, 0}),
    Visited = lists:reverse(get(all_visited)),
    BunnyLocation = first_double_visit(Visited),
    distance(BunnyLocation).

to_instructions(File) ->
    {ok, Bin} = file:read_file(File),
    Contents = binary_to_list(Bin) -- "\n",
    Tokens = string:tokens(Contents, ", "),
    [{Dir, list_to_integer(Steps)} || [Dir | Steps] <- Tokens].    

calculate_position([], _, Position) ->
    Position;
calculate_position([{Turn, Steps} | Rest], OldFacing, OldPosition) ->
    Facing = turn(OldFacing, Turn),
    Position = move(Steps, Facing, OldPosition),
    calculate_position(Rest, Facing, Position).

move(0, _, Pos) ->
    Pos;
move(Steps, Facing, {X, Y}) ->
    NewPos = case Facing of
		 north ->
		     {X, Y + 1};
		 south ->
		     {X, Y - 1};
		 east ->
		     {X + 1, Y};
		 west ->
		     {X - 1, Y}
	     end,
    Visited = get(all_visited),
    put(all_visited, [NewPos | Visited]),
    move(Steps - 1, Facing, NewPos).

turn(north, $R) -> east;
turn(north, $L) -> west; 
turn(east, $R) -> south; 
turn(east, $L) -> north; 
turn(south, $R) -> west;
turn(south, $L) -> east;
turn(west, $R) -> north;
turn(west, $L) -> south.

first_double_visit([H|T]) ->
    case lists:member(H, T) of
	true ->
	    H;
	false ->
	    first_double_visit(T)
    end.
    
distance({X, Y}) ->
    abs(X) + abs(Y).
