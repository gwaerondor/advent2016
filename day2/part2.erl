-module(part2).
-export([run/1]).
-define(UP_ALLOWED(X), not lists:member(X, [5, 2, 1, 4, 9])).
-define(DOWN_ALLOWED(X), not lists:member(X, [5, 10, 13, 12, 9])).
-define(LEFT_ALLOWED(X), not lists:member(X, [1, 2, 5, 10, 13])).
-define(RIGHT_ALLOWED(X), not lists:member(X, [1, 4, 9, 12, 13])).

run(File) ->
    Contents = read_contents(File),
    StartKey = 5,
    Values = get_all_keys(Contents, StartKey),
    [to_key(V) || V <- Values].

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

move_is_valid(Current, up) ->
    ?UP_ALLOWED(Current);
move_is_valid(Current, down) ->
    ?DOWN_ALLOWED(Current);
move_is_valid(Current, left) ->
    ?LEFT_ALLOWED(Current);
move_is_valid(Current, right) ->
    ?RIGHT_ALLOWED(Current).

move(3, up) -> 1;
move(6, up) -> 2;
move(7, up) -> 3;
move(8, up) -> 4;
move(10, up) -> 6;
move(11, up) -> 7;
move(12, up) -> 8;
move(13, up) -> 11;

move(1, down) -> 3;
move(2, down) -> 6;
move(3, down) -> 7;
move(4, down) -> 8;
move(6, down) -> 10;
move(7, down) -> 11;
move(8, down) -> 12;
move(11, down) -> 13;

move(Pos, left) -> Pos - 1;
move(Pos, right) -> Pos + 1.

to_key(N) when N > 9 ->
    [N - 10 + $A];
to_key(N) ->
    integer_to_list(N).
   
