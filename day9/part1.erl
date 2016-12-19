-module(part1).
-export([run/1]).

run(File) ->
    Compressed = parse_file(File),
    Decompressed = decompress(Compressed),
    length(Decompressed).

parse_file(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin) -- "\n".

decompress(Data) ->
    decompress(Data, []).

decompress([], Acc) ->
    Acc;
decompress(L = [$( | _], Acc) ->
    Spec_string = find_spec_string(L),
    Remaining = remove(Spec_string, L),
    Spec = to_spec(Spec_string),
    Remove_once_spec = {element(1, Spec), 1},
    To_add = apply_spec(Remove_once_spec, Remaining),
    Remaining_after_application = remove(To_add, Remaining),
    New_acc = Acc ++ apply_spec(Spec, To_add),
    decompress(Remaining_after_application, New_acc);
decompress([Char | Rest], Acc) ->
    decompress(Rest, Acc ++ [Char]).

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
