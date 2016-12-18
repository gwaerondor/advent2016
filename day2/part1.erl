-module(part1).
-export([run/1]).

run(File) ->
    Contents = read_contents(File),
    StartKey = 5,
    get_all_keys(Contents, StartKey).

read_contents(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    string:tokens(String, "\n").    

get_all_keys([], _) ->
    [];
get_all_keys([Sequence | R], StartPos) ->
    Value = next_key(StartPos, Sequence),
    [Value | get_all_keys(R, Value)].

next_key(Pos, []) ->
    Pos;
next_key(Pos, [M|R]) ->
    Movement = to_movement(M),
    V = case move_is_valid(Pos, Movement) of
	    true ->
		move(Pos, Movement);
	    false ->
		Pos
	end,
    next_key(V, R).

to_movement($U) -> up;
to_movement($D) -> down;
to_movement($L) -> left;
to_movement($R) -> right.

move_is_valid(1, up) -> false;
move_is_valid(2, up) -> false; 
move_is_valid(3, up) -> false; 
move_is_valid(1, left) -> false;
move_is_valid(4, left) -> false;
move_is_valid(7, left) -> false;
move_is_valid(3, right) -> false;
move_is_valid(6, right) -> false;
move_is_valid(9, right) -> false;
move_is_valid(7, down) -> false;
move_is_valid(8, down) -> false;
move_is_valid(9, down) -> false;
move_is_valid(_, _) -> true.

move(Pos, up) -> Pos - 3;
move(Pos, down) -> Pos + 3;
move(Pos, left) -> Pos - 1;
move(Pos, right) -> Pos + 1.
    
