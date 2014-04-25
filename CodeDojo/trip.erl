-module(trip).
-compile([export_all]).

trip(S) ->
    minimum_exchanges(S).

minimum_exchanges(S) ->
    Total = lists:sum(S),
    Avg = Total div length(S),
    R = Total rem length(S),
    (lists:sum(lists:map(fun(X) -> abs(X - Avg) end, S)) - R) div 2.
