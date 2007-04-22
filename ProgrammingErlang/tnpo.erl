-module(tnpo).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

nn(1) ->
    1;
nn(N) when N rem 2 =:= 0 ->
    N div 2;
nn(N) ->
    3 * N + 1.

nn_test_() ->
    [?_assertMatch(1, nn(1)),
     ?_assertMatch(10, nn(3)),
     ?_assertMatch(2, nn(4))].

cycle(1, CycleLength) ->
    CycleLength;
cycle(N, CycleLength) ->
    cycle(nn(N),CycleLength+1).
    

cycle(1, CycleLength, _) ->
    CycleLength;
cycle(N, CycleLength, Fun) ->
    Fun(nn(N),CycleLength+1, Fun).

cycle_test_() ->
    [?_assertMatch(1, cycle(1, 1, fun cycle/3)),
     ?_assertMatch(2, cycle(2, 1)),
     ?_assertMatch(11, cycle(2, 10)),
     ?_assertMatch(6, cycle(5, 1))].

retrieve(Key, Cache) ->
    case dict:find(Key, Cache) of
	{ok, Value} ->
	    Value;
	error ->
	    undefined
    end.

retrieve_not_found_test() ->
    Cache = dict:new(),
    ?assertMatch(undefined, retrieve(10, Cache)).

retrieve_found_test() ->
    Cache = dict:from_list([{3, 5}]),
    ?assertMatch(5, retrieve(3, Cache)).

put(Key, Value, Cache) ->
    dict:store(Key, Value, Cache).

put_test() ->
    Cache = dict:new(),
    ?assertMatch(10, retrieve(3, put(3, 10, Cache))).
