-module(exercise_3_2).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

monitor(F, Called) ->
    receive
	{call, From, Args} ->
	    From ! apply(F, Args),
	    monitor(F, Called + 1);
	{how_many_calls, From} ->
	    From ! Called,
	    monitor(F, Called)
    end.

make_monitored(F) ->
    Pid = spawn(?MODULE, monitor, [F, 0]),
    Receive = fun() ->
		      receive
			  R -> R
		      end
	      end,
    fun(how_many_calls) ->
	    Pid ! {how_many_calls, self()},
	    Receive();
       (Args) ->
	    Pid ! {call, self(), Args},
	    Receive()
    end.

monitor_test_() ->
    S = make_monitored(fun math:sqrt/1),
    [?_assertMatch(true, 10 == S([100])),
     ?_assertMatch(1, S(how_many_calls))].
