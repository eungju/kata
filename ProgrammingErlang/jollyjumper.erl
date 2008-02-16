-module(jollyjumper).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

is_stair([], _) ->
    true;
is_stair([H|T], Prev) when H =:= Prev + 1 ->
    is_stair(T, H);
is_stair(_, _) ->
    false.

jolly(L) ->
    jolly(L, []).
jolly([A,B|L], Acc) ->
    jolly([B|L], [abs(A-B)|Acc]);
jolly([_], Acc) ->
    is_stair(lists:sort(Acc), 0).

jollyjumper_test_() ->
    [?_assertMatch(true, jolly([1,4,2,3])),
     ?_assertMatch(false, jolly([2,4,7,11,16])),
     ?_assertMatch(true, jolly([1]))].
