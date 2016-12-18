-module(part1).
-export([run/1]).

run(File) ->
    IPs = parse_ips(File),
    Supported = [ IP || IP <- IPs, tls_supported(IP) ],
    length(Supported).    

parse_ips(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    string:tokens(String, "\n").

tls_supported(IP) ->
    Hypernets = get_hypernets(IP),
    Hypernet_ABBA_present = lists:any(fun contains_abba/1, Hypernets),
    case Hypernet_ABBA_present of
	true ->
	    false;
	false ->
	    has_abba_in_supernet(IP)
    end.

get_hypernets(IP) ->
    RE = "\\[(.*?)\\]",
    {match, Matches} = re:run(IP, RE, [{capture, all, list}, global]),
    [ lists:nth(2, Match) || Match <- Matches ].

has_abba_in_supernet(IP) ->
    Supernet = re:replace(IP, "\\[.*?\\]", " ", [{return, list}]),
    Parts = string:tokens(Supernet, " "),
    lists:any(fun contains_abba/1, Parts).

contains_abba(L) when length(L) < 4 ->
    false;
contains_abba([X, Y, Y, X | _]) when X /= Y ->
    true;
contains_abba([_ | Rest]) ->
    contains_abba(Rest).
