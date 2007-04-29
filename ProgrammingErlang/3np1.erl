-module('3np1').
-include_lib("eunit/include/eunit.hrl").
-export([max_cycle/1, max_cycle_upto/1, pmax_cycle/1, pmax_cycle_upto/1, benchmark/0]).

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

for(Func, L) ->
    lists:map(Func, lists:zip(lists:seq(1, length(L)), L)).

plan_split(NumberOfChunks, L) ->
    F = fun({Nth, X}) ->
		if Nth > (length(L) rem NumberOfChunks) ->
			X;
		   true ->
			X + 1
		end
	end,
    for(F, lists:duplicate(NumberOfChunks, length(L) div NumberOfChunks)).

plan_split_test_() ->
    [?_assertMatch([1, 1], plan_split(2, [1,2])),
     ?_assertMatch([2, 1], plan_split(2, [1,2,3])),
     ?_assertMatch([1, 0, 0], plan_split(3, [1]))].

split(NumberOfChunks, L) ->
    split(plan_split(NumberOfChunks, L), L, []).
split([], _, Acc) ->
    Acc;
split([H|T], L, Acc) ->
    {L1, L2} = lists:split(H, L),
    split(T, L2, [L1|Acc]).

split_test_() ->
    [?_assertMatch([[1,2], [3], [4]], lists:reverse(split(3, [1,2,3,4]))),
     ?_assertMatch([[1], [], []], lists:reverse(split(3, [1])))].

start_workers(N) ->
    lists:map(fun(_) -> spawn(fun worker/0) end, lists:seq(1, N)).

stop_workers(Workers) ->
    lists:foreach(fun(Worker) -> Worker ! stop end, Workers).

send_chunk(Worker, Chunk) ->
    Worker ! {self(), Chunk}.
    
pmax_cycle(L) ->
    Workers = start_workers(32),
    lists:foreach(fun({W, C}) -> send_chunk(W, C) end,
		  lists:zip(Workers, split(length(Workers), L))),
    stop_workers(Workers),
    reduce(length(Workers)).

pmax_cycle_upto(M) ->
    pmax_cycle(lists:seq(1, M)).

pmax_cycle_test() ->
    ?assertMatch(20, pmax_cycle_upto(10)).

benchmark() ->
    Args = [lists:seq(1, 1000000)],
    {T1, _} = timer:tc(?MODULE, max_cycle, Args),
    {T2, _} = timer:tc(?MODULE, pmax_cycle, Args),
    io:format("max_cycle: ~wms.~n", [T1 / 1000]),
    io:format("pmax_cycle: ~wms.~n", [T2 / 1000]).
			      
