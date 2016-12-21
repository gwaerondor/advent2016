-module(part1).
-export([run/1]).
-define(MOVE_COST, 2).
-define(AVOIDED_BACKTRACK, 3).

% My initial state
%    F4 .. .. .. .. .. .. .. .. .. ..
%    F3 .. .. .. CM .. cM .. RM .. PM
%    F2 .. .. CG .. cG .. RG .. PG ..
% E  F1 PG PM .. .. .. .. .. .. .. ..

% Input = pieces on each floor that need to be moved.
% For the above state, [2, 4, 4]

run(Input) ->
    Per_floor = cumsum(Input),
    lists:sum([?MOVE_COST * X || X <- Per_floor]) - ?AVOIDED_BACKTRACK.

cumsum(L) ->
    cumsum(L, 0).

cumsum([], _) ->
    [];
cumsum([H | T], Sum) ->
    S = Sum + H,
    [S | cumsum(T, S)].
