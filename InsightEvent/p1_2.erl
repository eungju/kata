-module(p1_2).
-compile(export_all).

s([_]=L) ->
    [[L]];
s([A|L]) ->
    S = s(L),
    lists:append([lists:map(fun(X) -> [[A]|X] end, S)|lists:map(fun(X) -> each_append(A,X) end, S)]).

each_append(_, [], _, Acc) ->
    Acc;
each_append(A, [H|L], Prefix, Acc) ->
    each_append(A, L, [H|Prefix], [lists:append(L,[[A|H]|Prefix])|Acc]).

each_append(A, L) ->
    each_append(A, L, [], []).

solve(N) ->
    s(lists:seq(0, N - 1)).
