-module(part2).
-export([run/1]).

run(File) ->
    Specifications = read_specifications(File),
    Transposed = transpose_specifications(Specifications),
    Valid_specs = [ Spec || Spec <- Transposed, is_valid(Spec) ],
    length(Valid_specs).

read_specifications(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Lines = string:tokens(String, "\n"),
    [ deep_list_to_integer(string:tokens(L, " ")) || L <- Lines ].

transpose_specifications(Specs) ->
    Groups = groups_of_three(Specs),
    Transposed_groups = [ transpose_group(G) || G <- Groups ],
    ungroup(Transposed_groups).
    
transpose_group(Group) ->
    [ [ lists:nth(N, G) || G <- Group]  || N <- [1, 2, 3] ].

groups_of_three([]) ->
    [];
groups_of_three([A, B, C | Rest]) ->
    [[A, B, C] | groups_of_three(Rest)].

ungroup(Deep_list) ->
    lists:append(Deep_list).

is_valid([X, Y, Z]) ->
    X + Y > Z andalso
    X + Z > Y andalso
    Y + Z > X.

deep_list_to_integer(List_of_lists) ->
    [ list_to_integer(L) || L <- List_of_lists ].
