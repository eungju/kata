-module(creditcard).
-export([type/1, luhn/1]).
-import(lists, [map/2, zip/2]).
-include_lib("eunit/include/eunit.hrl").

onetwos(N) ->
    onetwos(N, []).

onetwos(0, Acc) ->
    Acc;
onetwos(N, Acc) ->
    onetwos(N - 1, [2 - (N rem 2)|Acc]).

onetwos_test_() ->
    [?_assertMatch([], onetwos(0)),
     ?_assertMatch([1], onetwos(1)),
     ?_assertMatch([1,2,1], onetwos(3)),
     ?_assertMatch([1,2,1,2], onetwos(4))].

to_digits(Str) ->    
    [C - $0 || C <- Str, C >= $0, C =< $9].

to_digits_test_() ->
    [?_assertMatch([1,2,3], to_digits("123")),
     ?_assertMatch([0,1,2,3], to_digits("01 23"))].

luhn_digit(N) when N > 9 ->
    N - 9;
luhn_digit(N) ->
    N.

luhn(Digits) ->
    lists:sum(map(fun({X, Y}) -> luhn_digit(X * Y) end,
		  zip(Digits, lists:reverse(onetwos(length(Digits)))))) rem 10 =:= 0.

luhn_test_() ->
    [?_assert(luhn(to_digits("4408 0412 3456 7893"))),
     ?_assert(luhn(to_digits("4438 2412 3456 7893"))),
     ?_assert(not luhn(to_digits("4417 1234 5678 9112")))].

luhn() ->
    fun(Digits) ->
	    luhn(Digits)
    end.

number_length(Length) when is_integer(Length) ->
    fun(Digits) ->
	    length(Digits) =:= Length
    end;
number_length(Lengths) ->
    any(map(fun number_length/1, Lengths)).

number_length_test_() ->
    P = number_length(3),
    [?_assert(P([1,2,3])),
     ?_assert(not P([1,2]))].

number_length_or_test_() ->
    P = number_length([3, 5]),
    [?_assert(P([1,2,3])),
     ?_assert(P([1,2,3,4,5])),
     ?_assert(not P([1,2]))].

begins_with(Prefix) when is_integer(Prefix) ->
    fun(Digits) ->
	    lists:prefix([C - $0 || C <- integer_to_list(Prefix)], Digits)
    end;
begins_with(Prefixs) ->
    any(map(fun begins_with/1, Prefixs)).

begins_with_test_() ->
    P = begins_with(34),
    [?_assert(P([3,4,5])),
     ?_assert(not P([3,5,6]))].

begins_with_or_test_() ->
    P = begins_with([34, 44]),
    [?_assert(P([3,4,5])),
     ?_assert(P([4,4,5])),
     ?_assert(not P([3,5,6]))].

range(S, E) ->
    lists:seq(S, E).

range_test_() ->
    [?_assertMatch([3,4], range(3, 4))].

all(Predicates) ->
    fun(Digits) ->
	    lists:foldl(fun(P, Acc) -> P(Digits) and Acc end,
			true, Predicates)
    end.

any(Predicates) ->
    fun(Digits) ->
	    lists:foldl(fun(P, Acc) -> P(Digits) or Acc end,
			false, Predicates)
    end.
    
card_type(Name, Discriminants) ->
    {Name, all(Discriminants)}.

card_types() ->
    [card_type(amex, [begins_with([34,37]), number_length(15)]),
     card_type(discover, [begins_with(6011), number_length(16)]),
     card_type(mastercard, [begins_with(range(51, 55)), number_length(16)]),
     card_type(visa, [begins_with(4), number_length([13,16])])].

type(CardNumber) ->
    type(CardNumber, card_types()).
type(CardNumber, CardTypes) ->
    type(to_digits(CardNumber), CardTypes, []).
type(_Digits, [], []) ->
    unknown;
type(_Digits, [], [Matched]) ->
    Matched;
type(_Digits, [], Matched) ->
    {error, Matched};
type(Digits, [{Name, Discriminant}|Rest], Matched) ->
    case Discriminant(Digits) of
	true ->
	    type(Digits, Rest, [Name|Matched]);
	_ ->
	    type(Digits, Rest, Matched)
    end.

type_test_() -> 
    [?_assertMatch(visa, type("4408 0412 3456 7893")),
     ?_assertMatch(visa, type("4417 1234 5678 9112")),
     ?_assertMatch({error, [visa, acme]}, type("4408 0412 3456 7893", [card_type(acme, [])|card_types()]))].
