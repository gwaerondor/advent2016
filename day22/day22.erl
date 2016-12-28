-module(day22).
-export([run/0]).
-record(fs, {coord, size, used, avail}).
run() ->
    FS = parse_file_systems("input.txt"),
    length(lists:flatten(find_all_pairs(FS, FS))).

parse_file_systems(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    [_, _ | FS] = string:tokens(String, "\n"),
    FSs = [ make_file_system(F) || F <- FS ],
    lists:zip(lists:seq(1, length(FSs)), FSs).

make_file_system(Description) ->
    [Name, Size, Used, Avail, _] = string:tokens(Description, " "),
    #fs{coord = parse_coord(Name),
	size = parse_capacity(Size),
	used = parse_capacity(Used),
	avail = parse_capacity(Avail)}.

parse_capacity(Str) ->
    list_to_integer(Str -- "T").

parse_coord(Name) ->
    [X, Y] = get_match_groups(Name, "x(\\d+)-y(\\d+)"),
    {list_to_integer(X), list_to_integer(Y)}.    

get_match_groups(Subject, Regex) ->
    case re:run(Subject, Regex, [{capture, all, list}]) of
	{match, M} ->
	    [_ | Groups] = M,
	    Groups;
	_ ->
	    []
    end.

find_all_pairs([], _) ->
    [];
find_all_pairs([H|T], All) ->
    [ find_pairs(H, All) | find_all_pairs(T, All) ].

find_pairs({_, F}, _) when F#fs.used == 0 ->
    [];
find_pairs(_, []) ->
    [];
find_pairs({SN, Source}, [{TN, Target} | T]) when SN /= TN ->
    Source_used = Source#fs.used,
    Target_avail = Target#fs.avail,
    case Source_used =< Target_avail of
	true ->
	    [{SN, TN} | find_pairs({SN, Source}, T)];
	false ->
	    find_pairs({SN, Source}, T)
    end;
find_pairs(S, [_|T]) ->
    find_pairs(S, T).
