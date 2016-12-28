-module(day17).
-export([run/0, run/2]).

run() ->
    run("bwnlcvfs", []).

run(Input, Steps) ->
    Pos = to_pos(Steps, {1, 1}),
    io:format("Current position: ~p~n", [Pos]),
    Hash = hash(Input ++ Steps),
    draw_map(Hash, Pos).

draw_map(Hash, Pos) ->
    Door_states = door_states(Hash, Pos),
    io:format("Number of doors in states: ~p~n", [length(Door_states)]),
    io:format("#########~n"
	      "# ~s ~s ~s #~n"
              "#~s#~s#~s#~s#~n"
	      "# ~s ~s ~s #~n"
	      "#~s#~s#~s#~s#~n"
	      "# ~s ~s ~s #~n"
	      "#~s#~s#~s#~s#~n"
	      "# ~s ~s ~s  ~n"
	      "####### V~n", Door_states).

door_states(Hash, {Col, Row}) ->
    Verticals = [ C || C <- [Col-1, Col], C > 0, C < 4 ],
    Horizontals = [ H || H <- [Row-1, Row], H > 0, H < 4],
    Doors = [{vert, [c, c, c]},
	     {hor, [c, c, c, c]},
	     {vert, [c, c, c]},
	     {hor, [c, c, c, c]},
	     {vert, [c, c, c]},
	     {hor, [c, c, c, c]},
	     {vert, [c, c, c]}],
    doors_to_strings(Doors).

doors_to_strings([]) ->
    [];
doors_to_strings([{vert, L}|R]) ->
    Res = lists:map(fun(c) -> "|";
		       (o) -> " " end, L),
    Res ++ doors_to_strings(R);
doors_to_strings([{hor, L}|R]) ->
    Res = lists:map(fun(c) -> "-";
		       (o) -> " " end,
		      L),
    Res ++ doors_to_strings(R).

to_pos([], Pos) -> Pos;
to_pos([$U|R], {X, Y}) -> to_pos(R, {X, Y-1});
to_pos([$D|R], {X, Y}) -> to_pos(R, {X, Y+1});
to_pos([$L|R], {X, Y}) -> to_pos(R, {X-1, Y});
to_pos([$R|R], {X, Y}) -> to_pos(R, {X-1, Y}).


hash(Input) ->
    Bin = list_to_binary(Input),
    Encrypted = crypto:hash(md5, Bin),
    bin_to_hex(Encrypted).

%% bin_to_hex credit: stack overflow user jmuc from this thread:
%% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
bin_to_hex(Bin) ->
    binary_to_list(<< <<(hex(H)), (hex(L))>> || <<H:4, L:4>> <= Bin >>).

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

