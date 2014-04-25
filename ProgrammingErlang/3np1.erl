-module('3np1').
-include_lib("eunit/include/eunit.hrl").
-export([max_cycle/1, max_cycle_upto/1]).
-export([pmax_cycle/1, pmax_cycle_upto/1]).
-export([dmax_cycle/2, dmax_cycle_upto/2]).
-export([benchmark/0]).

cycle(N) ->
    cycle(N, 0).
			
cycle(1, Acc) ->
    Acc + 1;

cycle(N, Acc) when N rem 2 =:= 0 ->
    cycle(N div 2, Acc + 1);
cycle(N, Acc) ->
    cycle(N * 3 + 1, Acc + 1).

cycle_test_() ->
    [?_assertMatch(1, cycle(1)),
     ?_assertMatch(2, cycle(2)),
     ?_assertMatch(6, cycle(5))].

max_cycle(L) ->
    lists:max(lists:map(fun cycle/1, L)).

max_cycle_upto(M) ->
    max_cycle(lists:seq(1, M)).
    
max_cycle_test_() ->
    [?_assertMatch(1, max_cycle([1])),
     ?_assertMatch(2, max_cycle([1, 2])),
     ?_assertMatch(20, max_cycle_upto(10))].

reduce(M) ->
    reduce(M, 0).
reduce(0, Max) ->
    Max;
reduce(M, Max) ->
    receive
	{'EXIT', _, _} ->
	    reduce(M, Max);
	Cycle when Cycle > Max ->
	    reduce(M - 1, Cycle);
	_ ->
	    reduce(M - 1, Max)
    end.

reduce_test_() ->
    self() ! 1,
    self() ! 1,
    self() ! 2,
    self() ! 1,
    self() ! 2,
    [?_assertMatch(0, reduce(0)),
     ?_assertMatch(1, reduce(1)),
     ?_assertMatch(2, reduce(2)),
     ?_assertMatch(2, reduce(2))].

worker() ->
    receive
	{From, []} ->
	    From ! 0,
	    worker();
	{From, L} ->
	    From ! max_cycle(L),
	    worker();
	stop ->
	    void
    end.

worker_test() ->
    Pid = spawn(fun worker/0),
    Pid ! {self(), [1]},
    Pid ! stop,
    Result = receive
		 1 ->
		     true;
		 _ ->
		     false
	     end,
    ?assertMatch(true, Result).

distribute_fairly(Whole, NumberOfChunks) ->
    D = Whole div NumberOfChunks,
    R = Whole rem NumberOfChunks,
    lists:duplicate(R, D + 1) ++ lists:duplicate(NumberOfChunks - R, D).

distribute_fairly_test_() ->
    [?_assertMatch([1, 1], distribute_fairly(2, 2)),
     ?_assertMatch([2, 1], distribute_fairly(3, 2)),
     ?_assertMatch([1, 0, 0], distribute_fairly(1, 3))].

divide_into(L, NumberOfChunks) ->
    divide_into(L, distribute_fairly(length(L), NumberOfChunks), []).
divide_into(_, [], Acc) ->
    Acc;
divide_into(L, [H|T], Acc) ->
    {L1, L2} = lists:split(H, L),
    divide_into(L2, T, [L1|Acc]).

divide_into_test_() ->
    [?_assertMatch([[1,2], [3], [4]], lists:reverse(divide_into([1,2,3,4], 3))),
     ?_assertMatch([[1], [], []], lists:reverse(divide_into([1], 3)))].

send_chunk(Worker, Chunk) ->
    Worker ! {self(), Chunk}.
    
pmax_cycle(L) ->
    Workers = lists:map(fun(_) -> spawn_link(fun worker/0) end, lists:seq(1, 32)),
    lists:foreach(fun({W, C}) -> send_chunk(W, C) end,
		  lists:zip(Workers, divide_into(L, length(Workers)))),
    lists:foreach(fun(Worker) -> Worker ! stop end, Workers),
    reduce(length(Workers)).

pmax_cycle_upto(M) ->
    pmax_cycle(lists:seq(1, M)).

pmax_cycle_test() ->
    ?assertMatch(20, pmax_cycle_upto(10)).

dmax_cycle(Nodes, L) ->
    Workers = lists:map(fun(I) -> spawn_link(lists:nth((I - 1) rem length(Nodes) + 1, Nodes), fun worker/0) end, lists:seq(1, 32)),
    lists:foreach(fun({W, C}) -> send_chunk(W, C) end,
		  lists:zip(Workers, divide_into(L, length(Workers)))),
    lists:foreach(fun(Worker) -> Worker ! stop end, Workers),
    reduce(length(Workers)).

dmax_cycle_upto(Nodes, M) ->
    dmax_cycle(Nodes, lists:seq(1, M)).

dmax_cycle_test() ->
    ?assertMatch(20, dmax_cycle_upto([node()], 10)).

benchmark() ->
    Args = lists:seq(1, 1000000),
    {T1, _} = timer:tc(?MODULE, max_cycle, [Args]),
    io:format("max_cycle: ~wms.~n", [T1 / 1000]),
    {T2, _} = timer:tc(?MODULE, pmax_cycle, [Args]),
    io:format("pmax_cycle: ~wms.~n", [T2 / 1000]),
    {T3, _} = timer:tc(?MODULE, dmax_cycle, [[s1@juno.local,s2@juno.local], Args]),
    io:format("dmax_cycle: ~wms.~n", [T3 / 1000]).			      
