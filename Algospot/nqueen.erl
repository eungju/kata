-module(nqueen).
-import(lists, []).
-compile([export_all]).

vertical({_, B}, {_, D}) -> B =:= D.
diagonal({A, B}, {C, D}) ->
    DR = abs(A - C),
    DC = abs(B - D),
    case {DR div DC, DR rem DC} of
        {1, 0} ->
            true;
        _ -> false
    end.
    
safe(P, PS) ->
    not lists:any(fun (X) -> vertical(P, X) orelse diagonal(P, X) end, PS).

solve(N, PS) ->
    if length(PS) =:= N ->
            1;
       true ->
            CANDIDATES = lists:map(fun (C) -> {length(PS), C} end,
                                   lists:seq(0, N - 1)),
            lists:foldl(fun (P, Acc) -> solve(N, [P|PS]) + Acc end, 0, lists:filter(fun (P) -> safe(P, PS) end, CANDIDATES))
    end.

solve(N) ->
    solve(N, []). 
