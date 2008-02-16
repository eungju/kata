-module(jollyjumper).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

distance(L) ->
    distance(L, []).
distance([A,B|L], Acc) ->
    distance([B|L], [abs(A-B)|Acc]);
distance([_], Acc) ->
    Acc.

jollyjumper(L) ->
    lists:seq(1, length(L) - 1) =:= lists:sort(distance(L)).

jollyjumper_test_() ->
    [?_assertMatch(true, jollyjumper([1,4,2,3])),
     ?_assertMatch(false, jollyjumper([2,4,7,11,16]))].
