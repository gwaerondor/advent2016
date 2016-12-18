-module(part1).
-export([run/1]).

-record(room, {encrypted_name,
	       letters = [],
	       checksum,
	       sector}).

run(File) ->
    Rooms = scan_rooms(File),
    Valid = [ Room || Room <- Rooms, is_valid_room(Room) ],
    lists:foldl(fun(Room, Sum) -> Sum + Room#room.sector end, 0, Valid).

scan_rooms(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(Bin),
    Lines = string:tokens(String, "\n"),
    [ make_room(L) || L <- Lines ].

make_room(Crypto) ->
    Reversed = lists:reverse(Crypto),
    Checksum = lists:reverse(string:substr(Reversed, 2, 5)),
    Sector = list_to_integer(lists:reverse(string:substr(Reversed, 8, 3))),
    Remaining = lists:reverse(string:substr(Reversed, 12)),
    Letters = count_occurences(Remaining, []),
    #room{checksum = Checksum,
	  sector = Sector,
	  letters = Letters,
	  encrypted_name = Remaining}.

is_valid_room(Room) ->
    Letters = Room#room.letters,
    Most_common = sort_most_common(Letters),
    Five_most_common = [lists:nth(N, Most_common) || N <- lists:seq(1, 5)],
    Calculated_checksum = lists:flatten([ K || {K, _} <- Five_most_common ]),
    Checksum = Room#room.checksum,
    Calculated_checksum == Checksum.

sort_most_common(Letters) ->
    F = fun({K1, V1}, {K2, V2}) ->
		case V1 == V2 of
		    true ->
			K1 =< K2;
		    false ->
			V1 > V2
		end
	end,
    lists:sort(F, Letters).	   

count_occurences([], Occurences) ->
    Occurences;
count_occurences([L | R], Occurences) ->
    New_occurences = update_occurences([L], Occurences),
    count_occurences(R, New_occurences).

update_occurences("-", Occurences) ->
    Occurences;
update_occurences(Letter, Occurences) ->
    case proplists:get_value(Letter, Occurences) of
	undefined ->
	    [{Letter, 1} | Occurences];
	N ->
	    lists:keyreplace(Letter, 1, Occurences, {Letter, N + 1})
    end.
