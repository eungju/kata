-module(credit).
-export([]).
-include_lib( "eunit/include/eunit.hrl" ).


is_valid( CreditCardNumber ) ->
%  CreditCardNumber,
%  true.
	is_valid(lists:reverse(CreditCardNumber),[],1). 

is_valid(CreditCardNumber,Result,N) ->
    	
	true.

to_digits(S) ->
	[X - $0 || X <- S, not(X == $ ) ].

transdigit(L) ->
    reverse(transdigit(reverse(L),1)).

transdigit(L,N) ->
    lists:map(fun cal/2, 

    

cal(N, Pos) when Pos rem 2 == 0 ->
    N * 2;
cal(N, Pos) when Pos rem 2 == 1 ->
    N.

credit_test_() -> [
	?_assert( is_valid("4408 0412 3456 7893")),
	?_assertMatch([4,4,0,8,0,4,1,2,3,4,5,6,7,8,9,3],
		to_digits("4408 0412 3456 7893")),
	?_assertMatch([1*2,2,3*2,4],
		transdigit([1,2,3,4]))
].
