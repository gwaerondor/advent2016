-module(part2).
-export([run/1]).

run(File) ->
    IPs = parse_IPs(File),
    Supported = [ IP || IP <- IPs, ssl_supported(IP) ],
    length(Supported).

parse_IPs(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    string:tokens(String, "\n").

ssl_supported(IP) ->
    Hypernets = get_hypernets(IP),
    Supernets = get_supernets(IP),
    ABAs = get_all_xyx(Supernets),
    BABs = get_all_xyx(Hypernets),
    lists:any(fun([A,B,A]) -> lists:member([B,A,B], BABs) end, ABAs).

get_hypernets(IP) ->
    RE = "\\[(.*?)\\]",
    {match, Matches} = re:run(IP, RE, [{capture, all, list}, global]),
    [ lists:nth(2, Match) || Match <- Matches ].

get_supernets(IP) ->
    Supernets = re:replace(IP, "\\[.*?\\]", " ", [{return, list}, global]),
    string:tokens(Supernets, " ").

get_all_xyx(Strings) ->
    lists:foldl(fun(S, Acc) -> get_xyx(S, []) ++ Acc end,
		[], Strings).

get_xyx(L, Acc) when length(L) < 3 ->
    Acc;
get_xyx([X, Y, X | Rest], Acc) when X /= Y ->
    get_xyx([Y, X | Rest], [[X, Y, X] | Acc]);
get_xyx([_ | Rest], Acc) ->
    get_xyx(Rest, Acc).
