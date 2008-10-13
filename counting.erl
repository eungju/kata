-module(counting).
-compile([export_all]).

gustavo(D) when 1 =< D, D =< 3 ->
    D;
gustavo(D) when D =:= 4 ->
    1.

count(N) ->
    count(N, [], 0).
count(N, Trial, Count) ->
    %%io:format("~p~n", [Trial]),
    Sum = lists:sum(lists:map(fun gustavo/1, Trial)),
    if
	Sum > N ->
	    Count + 0;
	Sum =:= N ->
	    Count + 1;
	true ->
	    lists:foldl(fun(X, S) -> count(N, [X|Trial], S) end, Count, lists:seq(1, 4))
    end.

fac(N) ->
    fac(N, 1).
fac(0, Acc) ->
    Acc;
fac(N, Acc) ->
    fac(N - 1, N * Acc).

combination(A, B, C, D) ->
    fac(A + B + C + D) div (fac(A) * fac(B) * fac(C) * fac(D)).

count_fast(N) ->
    lists:sum([combination(A, B, C, D) || A <- lists:seq(0, N), B <- lists:seq(0, N - A), C <- lists:seq(0, N - A - B), D <- lists:seq(0, N - A - B - C), (A + 2 * B + 3 * C + D) =:= N]).
