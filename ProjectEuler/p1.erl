-module(p1).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

is_multiple(N) when N rem 3 =:= 0; N rem 5 =:= 0 ->
    true;
is_multiple(_) ->
    false.
    
sum(1, S) ->
    S; 
sum(I, S) ->
    case is_multiple(I) of
	true ->
	    sum(I - 1, S + I);
	_ ->
	    sum(I - 1, S)
    end.

sum(Below) ->
    sum(Below - 1, 0).

is_multiple_test_() ->
    [?_assertMatch(false, is_multiple(1)),
     ?_assertMatch(true, is_multiple(3)),
     ?_assertMatch(true, is_multiple(5)),
     ?_assertMatch(true, is_multiple(6)),
     ?_assertMatch(true, is_multiple(10))].

sum_test_() ->
    [?_assertMatch(23, sum(10))].

problem_test() ->
     ?assertMatch(233168, sum(1000)).
