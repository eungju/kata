-module('3np1').
-include_lib("eunit/include/eunit.hrl").
-export([max_cycle/1, pmax_cycle/1]).

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

max_cycle(M) ->
    lists:max(lists:map(fun cycle/1, lists:seq(1, M))).

max_cycle_test_() ->
    [?_assertMatch(1, max_cycle(1)),
     ?_assertMatch(2, max_cycle(2)),
     ?_assertMatch(20, max_cycle(10))].

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

worker() ->
    receive
	{From, N} ->
	    From ! {N, cycle(N)},
	    worker();
	stop ->
	    void
    end.

worker_test() ->
    Pid = spawn(fun worker/0),
    Pid ! {self(), 1},
    Pid ! stop,
    Result = receive
		 {1, 1} ->
		     true;
		 _ ->
		     false
	     end,
    ?assertMatch(true, Result).

pmax_cycle(M) ->
    L = lists:seq(1, M),
    NWorkers = 10,
    Workers = lists:map(fun(_) -> spawn(fun worker/0) end, lists:seq(1, NWorkers)),
    lists:map(fun(N) -> lists:nth((N rem NWorkers) + 1, Workers) ! {self(), N} end, L),
    lists:map(fun(Worker) -> Worker ! stop end, Workers),
    reduce(L).
    
pmax_cycle_test() ->
    ?assertMatch(20, pmax_cycle(10)).
