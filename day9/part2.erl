-module(part2).
-export([run/1]).

run(File) ->
    Compressed = parse_file(File),
    decompressed_size(Compressed, 0).

parse_file(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin) -- "\n".

decompressed_size([], Size) ->
    Size;
decompressed_size(L = [$( | _], Size) ->
    Spec_string = find_spec_string(L),
    Remaining = remove(Spec_string, L),
    Spec = to_spec(Spec_string),
    Remove_once_spec = {element(1, Spec), 1},
    To_remove = apply_spec(Remove_once_spec, Remaining),
    After_removing_once = remove(To_remove, Remaining),
    To_add = apply_spec(Spec, Remaining),
    New_list = To_add ++ After_removing_once,
    decompressed_size(New_list, Size);
decompressed_size([_ | Rest], Size) ->
    decompressed_size(Rest, Size + 1).

find_spec_string(L) ->
    Last = string:str(L, ")"),
    string:substr(L, 1, Last).

to_spec(String) ->
    S = String -- "()",
    [Char_count, Repetitions] = string:tokens(S, "x"),
    C = list_to_integer(Char_count),
    R = list_to_integer(Repetitions),
    {C, R}.

apply_spec({Count, Repetitions}, String) ->
    Subject = string:substr(String, 1, Count),
    string:copies(Subject, Repetitions).

remove([], List) ->
    List;
remove([X|R1], [X|R2]) ->
    remove(R1, R2).
