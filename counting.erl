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
	    lists:sum(lists:map(fun(X) -> count(N, [X|Trial], Count) end, lists:seq(1, 4)))
    end.
