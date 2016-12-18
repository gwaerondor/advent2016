-module(part1).
-export([run/1]).

run(File) ->
    Instructions = to_instructions(File),
    Coordinates = calculate_position(Instructions, north, {0, 0}),
    distance(Coordinates).

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

move(Steps, Facing, {X, Y}) ->
    case Facing of
	north ->
	    {X, Y + Steps};
	south ->
	    {X, Y - Steps};
	east ->
	    {X + Steps, Y};
	west ->
	    {X - Steps, Y}
    end.

turn(north, $R) -> east;
turn(north, $L) -> west; 
turn(east, $R) -> south; 
turn(east, $L) -> north; 
turn(south, $R) -> west;
turn(south, $L) -> east;
turn(west, $R) -> north;
turn(west, $L) -> south.

distance({X, Y}) ->
    abs(X) + abs(Y).
