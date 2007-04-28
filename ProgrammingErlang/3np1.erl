-module('3np1').
-include_lib("eunit/include/eunit.hrl").

cycle(N) ->
    cycle(N, 0).
cycle(1, Acc) ->
    Acc + 1;
cycle(N, Acc) ->
    if
	N rem 2 =:= 0 ->
	    cycle(N div 2, Acc + 1);
	true ->
	    cycle(N * 3 + 1, Acc + 1)
    end.

cycle_test_() ->
    [?_assertMatch(1, cycle(1)),
     ?_assertMatch(2, cycle(2)),
     ?_assertMatch(6, cycle(5))].

reduce(L) ->
    reduce(L, 0).
reduce([], Max) ->
    Max;
reduce([H|T], Max) ->
    receive
	{H, Cycle} ->
	    if
		Cycle > Max ->
		    reduce(T, Cycle);
		true ->
		    reduce(T, Max)
	    end
    end.

reduce_test_() ->
    self() ! {1, 1},
    self() ! {1, 1},
    self() ! {2, 2},
    self() ! {1, 1},
    self() ! {2, 2},
    [?_assertMatch(0, reduce([])),
     ?_assertMatch(1, reduce([1])),
     ?_assertMatch(2, reduce([1, 2])),
     ?_assertMatch(2, reduce([2, 1]))
    ].
