-module(exercise_3_1).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

accumulator(Acc) ->
    receive
	{From, N} ->
	    NewAcc = Acc + N,
	    From ! NewAcc,
	    accumulator(NewAcc)
    end.

make_accumulator(I) ->
    Pid = spawn(?MODULE, accumulator, [I]),
    fun(N) ->
	    Pid ! {self(), N},
	    receive
		Acc ->
		    Acc
	    end
    end.

accumulator_test_() ->
    A = make_accumulator(5),
    [?_assertMatch(15, A(10)),
     ?_assertMatch(25, A(10))].

