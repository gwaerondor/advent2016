-module(part1).
-export([run/1]).
-record(bot, {chips = [],
	      low_target,
	      high_target}).

run(File) ->
    Rules = parse_rules(File),
    Insert_rule = fun(Rule, Bots) -> insert(Rule, Bots) end,
    Initial_state = lists:foldl(Insert_rule, [], Rules),
    start_bots(Initial_state).

parse_rules(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Rules = string:tokens(String, "\n"),
    [ to_rule(R) || R <- Rules ].

to_rule(R) ->
    case R of
	"value " ++ Rest ->
	    Numbers = re:replace(Rest,
				 "([a-z]|\\s)+",
				 " ",
				 [{return, list}, global]),
	    [Value, Bot] = string:tokens(Numbers, " "),
	    V = list_to_integer(Value),
	    B = "bot " ++ Bot,
	    {B, V, input};
	Rule ->
	    RE = "(.*) gives low to (.*) and high to (.*)",
	    {match, M} = re:run(Rule, RE, [{capture, all, list}, global]),
	    [[_, Bot, Low, High]] = M,
	    {Bot, Low, High, handover}
    end.

insert({B, V, input}, Bots) ->
    case proplists:get_value(B, Bots) of
	undefined ->
	    [{B, #bot{chips = [V]}} | Bots];
	Bot ->
	    New_chips = [V | Bot#bot.chips],
	    New_bot = Bot#bot{chips = New_chips},
	    lists:keyreplace(B, 1, Bots, {B, New_bot})
    end;
insert({B, Low, High, handover}, Bots) ->
    case proplists:get_value(B, Bots) of
	undefined ->
	    [{B, #bot{low_target = Low, high_target = High}} | Bots];
	Bot ->
	    New_bot = Bot#bot{low_target = Low, high_target = High},
	    lists:keyreplace(B, 1, Bots, {B, New_bot})
    end.

start_bots(State) ->
    case handles_special_case(State) of
	unknown ->
	    Moves = extract_moves(State),
	    Actual_moves = [ M || M <- Moves, M /= nop ],
	    New_state = execute_moves(Actual_moves, State),
	    start_bots(New_state);
	Bot_name ->
	    io:format("~p handles the special case.~n"
		      "Its description is: ~p~n",
		      [Bot_name, get_bot(Bot_name, State)]),
	    ok
    end.

extract_moves([]) ->
    [];
extract_moves([{Bot_name, Bot} | R]) ->
    Chips = Bot#bot.chips,
    case length(Chips) of
	2 ->
	    Move = extract_move_if_real_bot(Bot_name, Bot),
	    [Move | extract_moves(R)];
	_ ->
	    extract_moves(R)
    end.

extract_move_if_real_bot(Bot_name, Bot) ->
    case Bot_name of
	"output" ++ _ ->
	    nop;
	_ ->
	    Chips = Bot#bot.chips,
	    Low = lists:min(Chips),
	    High = lists:max(Chips),
	    Low_target = Bot#bot.low_target,
	    High_target = Bot#bot.high_target,
	    {Bot_name, {Low, Low_target}, {High, High_target}}
    end.

execute_moves([], State) ->
    State;
execute_moves([Move | R], State) ->
    {Source, {Low, Low_target}, {High, High_target}} = Move,
    Source_bot = get_bot(Source, State),
    High_bot = get_bot(High_target, State),
    Low_bot = get_bot(Low_target, State),
    New_source = Source_bot#bot{chips = []},
    New_high = add_chip(High, High_bot),
    New_low = add_chip(Low, Low_bot),
    S1 = lists:keyreplace(Source, 1, State, {Source, New_source}),
    S2 = lists:keyreplace(High_target, 1, S1, {High_target, New_high}),
    S3 = lists:keyreplace(Low_target, 1, S2, {Low_target, New_low}),
    execute_moves(R, S3).

add_chip(To_add, Bot) ->
    Current = Bot#bot.chips,
    Bot#bot{chips = [To_add | Current]}.

get_bot(Name, State) ->
    case proplists:get_value(Name, State) of
	undefined ->
	    #bot{};
	Bot ->
	    Bot
    end.

handles_special_case([]) ->
    unknown;
handles_special_case([{BN, B} | R]) ->
    Special = lists:sort([61, 17]),
    Actual = lists:sort(B#bot.chips),
    case Actual of
	Special ->
	    BN;
	_ ->
	    handles_special_case(R)
    end.
