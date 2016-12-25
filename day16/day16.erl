-module(day16).
-export([run/0, run/2]).

run() ->
    run("01111010110010011", 35651584).

run(Input, Desired_length) ->
    Data = generate_data(Input, Desired_length),
    Checksum = calculate_checksum(Data),
    io:format("Checksum is ~p~n", [Checksum]).

generate_data(Input, Desired_length) ->
    A = Input,
    Pre_B = lists:reverse(A),
    B = lists:map(fun($1) -> $0;
		     ($0) -> $1
		  end, Pre_B),
    Data = A ++ "0" ++ B,
    case length(Data) >= Desired_length of
	true ->
	    {R, _} = lists:split(Desired_length, Data),
	    R;
	false ->
	    generate_data(Data, Desired_length)
    end.

calculate_checksum(Data) ->
    CS = do_calculate_checksum(Data),
    Length = length(CS),
    case Length rem 2 of
	0 ->
	    calculate_checksum(CS);
	1 ->
	    CS
    end.

do_calculate_checksum([]) ->
    [];
do_calculate_checksum([X, X | Rest]) ->
    [$1 | do_calculate_checksum(Rest)];
do_calculate_checksum([_, _ | Rest]) ->
    [$0 | do_calculate_checksum(Rest)].
