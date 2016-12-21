-module(part1).
-export([run/1]).
-define(INIT_STATE, [{"a", 0}, {"b", 0}, {"c", 0}, {"d", 0}]).


run(File) ->
    Instructions = parse_instructions(File),
    execute(Instructions).

parse_instructions(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Tokens = string:tokens(String, "\n"),
    [ to_instruction(T) || T <- Tokens ].

to_instruction(String) ->
    Tokens = string:tokens(String, " "),
    Instruction = hd(Tokens),
    case Instruction of
	"cpy" ->
	    What = lists:nth(2, Tokens),
	    Target = lists:nth(3, Tokens),
	    {copy, What, Target};
	"inc" ->
	    What = lists:nth(2, Tokens),
	    {add, What, 1};
	"dec" ->
	    What = lists:nth(2, Tokens),
	    {add, What, -1};
	"jnz" ->
	    Cond = lists:nth(2, Tokens),
	    Steps = list_to_integer(lists:nth(3, Tokens)),
	    {jump, Cond, Steps}
    end.

execute(Instructions) ->
    execute(Instructions, 1, ?INIT_STATE).

execute(Instructions, Current, State) when Current > length(Instructions) ->
    State;
execute(Instructions, Current, State) ->
    Instruction = get_instruction(Current, Instructions),
    case Instruction of
	{jump, Cond, Steps} ->
	    case read_register(Cond, State) of
		0 ->
		    execute(Instructions, Current + 1, State);
		_ ->
		    execute(Instructions, Current + Steps, State)
	    end;
	I ->
	    New_state = apply_instruction(I, State),
	    execute(Instructions, Current + 1, New_state)
    end.

get_instruction(Number, Instructions) ->
    lists:nth(Number, Instructions).

apply_instruction(Instruction, State) ->
    case Instruction of
	{copy, What, Target} ->
	    Value = to_value(What, State),
	    write_register(State, Target, Value);
	{add, Register, Value} ->
	    Current = read_register(Register, State),
	    write_register(State, Register, Current  + Value)
    end.

read_register(Register, State) ->
    proplists:get_value(Register, State).

write_register(State, Register, Value) ->
    lists:keyreplace(Register, 1, State, {Register, Value}).
	
to_value(What, State) ->
    case catch list_to_integer(What) of
	{'EXIT', _} ->
	    read_register(What, State);
	X ->
	    X
    end.
