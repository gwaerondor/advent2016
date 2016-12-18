-module(part1).
-export([run/1]).

run(File) ->
    Specifications = read_specifications(File),
    Valid_specs = [ Spec || Spec <- Specifications, is_valid(Spec) ],
    length(Valid_specs).

read_specifications(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Lines = string:tokens(String, "\n"),
    [ deep_list_to_integer(string:tokens(L, " ")) || L <- Lines ].

is_valid([X, Y, Z]) ->
    X + Y > Z andalso
    X + Z > Y andalso
    Y + Z > X.

deep_list_to_integer(List_of_lists) ->
    [ list_to_integer(L) || L <- List_of_lists ].
