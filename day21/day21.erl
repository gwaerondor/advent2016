-module(day21).
-export([r/2, run/0, run/1]).
-define(INIT, "abcdefgh").
-define(SCRAMBLED, "fbgdceah").

run() ->
    run("input.txt").

run(File) ->
    Instructions = parse_instructions(File),
    Part_1 = lists:foldl(fun(I, Acc) ->
				 apply_instruction(I, Acc)
			 end, ?INIT, Instructions),
    io:format("Solution to part 1: ~p~n", [Part_1]),
    Part_2 = lists:foldl(fun(I, Acc) ->
				 revert_instruction(I, Acc)
			 end, ?SCRAMBLED, lists:reverse(Instructions)),
    io:format("[UNFINISHED: Solution to part 2: ~p]~n", [Part_2]).
				 
r(Instruction, String) ->
    apply_instruction(Instruction, String).

parse_instructions(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Lines = string:tokens(String, "\n"),
    [ make_instruction(L) || L <- Lines ].

make_instruction(String) ->
    case String of
	"swap position" ++ R ->
	    {N1, N2} = two_integers(R),
	    {swap_pos, N1 + 1, N2 + 1};
	"swap letter" ++ R ->
	    RE = "(\\w) with letter (\\w)",
	    [Letter_1, Letter_2] = get_match_groups(R, RE),
	    {swap_letters, Letter_1, Letter_2};
	"rotate based on position of letter " ++ R ->
	    {rotate_relative, R};
	"rotate left" ++ R ->
	    Steps = first_integer(R),
	    {rotate_left, Steps};
	"rotate right" ++ R ->
	    Steps = first_integer(R),
	    {rotate_right, Steps};
	"reverse" ++ R ->
	    {From, To} = two_integers(R),
	    {reverse, From + 1, To + 1};
	"move" ++ R ->
	    {From, To} = two_integers(R),
	    {move, From + 1, To + 1}
    end.

first_integer(String) ->
    RE = "(\\d+).*",
    [Number] = get_match_groups(String, RE),
    list_to_integer(Number).

two_integers(String) ->
    RE = "(\\d+).*(\\d+)",
    [N1, N2] = get_match_groups(String, RE),
    {list_to_integer(N1), list_to_integer(N2)}.

get_match_groups(Subject, Regex) ->
    case re:run(Subject, Regex, [{capture, all, list}]) of
	{match, M} ->
	    [_ | Groups] = M,
	    Groups;
	_ ->
	    []
    end.

apply_instruction({swap_pos, X, Y}, String) ->
    {Left,[F|Right]} = lists:split(X-1, String),
    LT = Left ++ [lists:nth(Y, String) | Right],
    {Left2,[_ | Right2]} = lists:split(Y-1, LT),
    Left2 ++ [F | Right2];
apply_instruction({swap_letters, A, B}, String) ->
    From = string:chr(String, hd(A)),
    To = string:chr(String, hd(B)),
    apply_instruction({swap_pos, From, To}, String);
apply_instruction({rotate_relative, Letter}, String) ->
    S = string:chr(String, hd(Letter)),
    Steps = case S > 4 of
		true ->
		    S + 1;
		false ->
		    S
	    end,
    Instruction = {rotate_right, Steps},
    apply_instruction(Instruction, String);
apply_instruction({rotate_right, Steps}, String) ->
    rotate_right(String, Steps);
apply_instruction({rotate_left, Steps}, String) ->
    rotate_left(String, Steps);
apply_instruction({reverse, From, To}, String) ->
    reverse_between(From, To, String);
apply_instruction({move, From, To}, String) ->
    move(From, To, String).

rotate_right(String, 0) ->
    String;
rotate_right(String, Steps) ->
    [Last | Rest] = lists:reverse(String),
    Res = [Last | lists:reverse(Rest)],
    rotate_right(Res, Steps - 1).

rotate_left(String, 0) ->
    String;
rotate_left([First | R], Steps) ->
    Reversed = [First | lists:reverse(R)],
    Res = lists:reverse(Reversed),
    rotate_left(Res, Steps - 1).

reverse_between(From, To, String) ->
    {Left, Right} = lists:split(From - 1, String),
    Length = To - (From - 1),
    Reversed = lists:reverse(string:substr(Right, 1, Length)),
    Remaining_right = Right -- Reversed,
    Left ++ Reversed ++ Remaining_right.

move(From, To, String) ->
    Elem = lists:nth(From, String),
    Reduced = lists:delete(Elem, String),
    insert_at(To, Elem, Reduced).

insert_at(1, Element, L) ->
    [Element | L];
insert_at(N, Element, [H|R]) ->
    [H | insert_at(N - 1, Element, R)].
    
%%% --------------------------------------------------------

revert_instruction(I = {swap_letters, _, _}, String) ->
    apply_instruction(I, String);
revert_instruction({rotate_right, Steps}, String) ->
    apply_instruction({rotate_left, Steps}, String);
revert_instruction({rotate_left, Steps}, String) ->
    apply_instruction({rotate_right, Steps}, String);
revert_instruction(_, String) ->
    String.
