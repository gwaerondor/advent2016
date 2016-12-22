-module(part1).
-export([run/1]).

run(Input) ->
    pretty_print([ lists:reverse(make_row(50, Y, Input)) || Y <- lists:seq(0, 50) ]).

make_row(-1, _, _) ->
    [];
make_row(X, Y, Input) ->
    [ is_open(X, Y, Input) | make_row(X-1, Y, Input) ].

is_open(1, 1, _) ->
    start;
is_open(31, 39, _) ->
    target;
is_open(X, Y, Input) ->
    N = X*X + 3*X + 2*X*Y + Y + Y*Y + Input,
    [Binary] = io_lib:format("~.2B", [N]),
    Ones = lists:foldl(fun($1, Sum) -> Sum + 1;
			  (_, Sum) -> Sum end,
		       0,
		       Binary),
    Ones rem 2 == 0.			

pretty_print([]) ->
    ok;
pretty_print([H|T]) ->
    Chars = [ to_char(Type) || Type <- H ],
    io:format("~s~n", [Chars]),
    pretty_print(T).

to_char(false) -> $#;
to_char(true) -> $.;
to_char(start) -> $o;
to_char(target) -> $x.
