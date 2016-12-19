-module(day8).
-export([run/1]).

-define(LINES, 6).
-define(COLUMNS, 50).

run(File) ->
    Instructions = read_instructions(File),
    Matrix = lists:foldl(fun(X, Acc) ->
				 apply_instruction(X, Acc)
			 end, init(), Instructions),
    pretty_print(Matrix),
    count_lit_pixels(Matrix).

read_instructions(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Instructions = string:tokens(String, "\n"),
    [ parse_instruction(I) || I <- Instructions ].
    
parse_instruction(String) ->
    case String of
	"rect " ++ Specs ->
	    [XS, YS] = string:tokens(Specs, "x"),
	    X = list_to_integer(XS),
	    Y = list_to_integer(YS),
	    {rect, X, Y};
	"rotate row y=" ++ Specs ->
	    parse_rotation(row, Specs);
	"rotate column x=" ++ Specs ->
	    parse_rotation(column, Specs)
    end.

parse_rotation(Type, String) ->
    [IndexS, StepsS] = string:tokens(String, " by "),
    Index = list_to_integer(IndexS) + 1,
    Steps = list_to_integer(StepsS),
    {Type, Index, Steps}.

apply_instruction(Instruction, Matrix) ->
    case Instruction of
	{rect, X, Y} ->
	    rect(X, Y, Matrix);
	{Rotation, N, Steps} ->
	    rotate(Rotation, N, Steps, Matrix)
    end.

rect(_, 0, Matrix) ->
    Matrix;
rect(X, Y, Matrix) ->
    Line = proplists:get_value(Y, Matrix),
    RE = ".{" ++ integer_to_list(X) ++ "}",
    Replacement = string:copies("#", X),
    New_line = re:replace(Line, RE, Replacement, [{return, list}]),
    Updated = lists:keyreplace(Y, 1, Matrix, {Y, New_line}),
    rect(X, Y - 1, Updated).

rotate(row, Row, Steps, Matrix) ->
    Line = proplists:get_value(Row, Matrix),
    New_line = do_rotation(Line, Steps),
    Updated = {Row, New_line},
    lists:keyreplace(Row, 1, Matrix, Updated);
rotate(column, Col, Steps, Matrix) ->
    Column = [ lists:nth(Col, L) || {_, L} <- Matrix ],
    New_col = do_rotation(Column, Steps),
    [ {K, replace_at(Col, L, lists:nth(K, New_col))} || {K, L} <- Matrix ].

replace_at(N, List, Replacement) ->
    {Left, [_ |Right]} = lists:split(N - 1, List),
    lists:append(Left, [Replacement | Right]).
    
do_rotation(List, 0) ->
    List;
do_rotation(List, Steps) ->
    Rev = lists:reverse(List),
    [ Last | Rest ] = Rev,
    Updated = [ Last | lists:reverse(Rest) ],
    do_rotation(Updated, Steps - 1).    

init() ->
    Line = string:copies(".", ?COLUMNS),
    Lines = lists:duplicate(?LINES, Line),
    Indices = lists:seq(1, ?LINES),
    lists:zip(Indices, Lines).

pretty_print(Matrix) ->
    [ io:format("~s~n", [L]) || {_, L} <- Matrix ].

count_lit_pixels(Matrix) ->
    Count_lit = fun($#, Acc) -> Acc + 1;
		   (_, Acc) -> Acc
		end,
    By_row = [ lists:foldl(Count_lit, 0, L) || {_, L} <- Matrix ],
    lists:sum(By_row).
