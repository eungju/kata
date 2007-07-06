-module(creditcard).
-export([type/1, luhn/1]).
-import(lists, [map/2, zip/2, reverse/1]).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

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

luhn_digit(N) when N > 9 ->
    N - 9;
luhn_digit(N) ->
    N.

luhn_checksum(Digits) ->
    lists:sum(map(fun({X, Y}) -> luhn_digit((X - $0) * Y) end,
		  zip(Digits, reverse(onetwos(length(Digits)))))).

luhn(Digits) ->
    luhn_checksum(Digits) rem 10 =:= 0.

luhn_test_() ->
    [?_assertMatch(3, luhn_checksum("3")),
     ?_assertMatch(7, luhn_checksum("23")),
     ?_assertMatch(7, luhn_checksum("80")),
     ?_assert(luhn("4408041234567893")),
     ?_assert(luhn("4438241234567893")),
     ?_assert(not luhn("4417123456789112"))].

luhn() ->
    fun(Digits) ->
	    luhn(Digits)
    end.

length_is(Length) when is_integer(Length) ->
    fun(Digits) ->
	    length(Digits) =:= Length
    end;
length_is(Lengths) when is_list(Lengths) ->
    satisfy_any(map(fun length_is/1, Lengths)).

length_is_test_() ->
    P = length_is(3),
    [?_assert(P("123")),
     ?_assert(not P("12"))].

length_is_one_of_test_() ->
    P = length_is([3, 5]),
    [?_assert(P("123")),
     ?_assert(P("12345")),
     ?_assert(not P("12"))].

begin_with([H|_]=Prefix) when is_integer(H) ->
    fun(Digits) ->
	    lists:prefix(Prefix, Digits)
    end;
begin_with([H|_]=Prefixes) when is_list(H) ->
    satisfy_any(map(fun begin_with/1, Prefixes)).

begin_with_range(S, E) ->
    Range = map(fun integer_to_list/1,
		lists:seq(list_to_integer(S), list_to_integer(E))),
    begin_with(Range).

begin_with_test_() ->
    P = begin_with("34"),
    [?_assert(P("345")),
     ?_assert(not P("356"))].

begin_with_one_of_test_() ->
    P = begin_with(["34", "44"]),
    [?_assert(P("345")),
     ?_assert(P("445")),
     ?_assert(not P("356"))].

satisfy_all(Predicates) ->
    fun(Digits) ->
	    lists:foldl(fun(P, Acc) -> P(Digits) and Acc end,
			true, Predicates)
    end.

satisfy_any(Predicates) ->
    fun(Digits) ->
	    lists:foldl(fun(P, Acc) -> P(Digits) or Acc end,
			false, Predicates)
    end.
    
card_type(Name, Discriminant) ->
    {Name, Discriminant}.

card_types() ->
    [card_type(amex,
	       satisfy_all([begin_with(["34","37"]), length_is(15)])),
     card_type(discover,
	       satisfy_all([begin_with("6011"), length_is(16)])),
     card_type(mastercard,
	       satisfy_all([begin_with_range("51", "55"), length_is(16)])),
     card_type(visa, satisfy_all([begin_with("4"), length_is([13,16])]))].

guess_type(Digits, CardTypes) ->
    [Name || {Name, Discriminant} <- CardTypes, Discriminant(Digits)].

type(Digits) ->
    type(Digits, card_types()).
type(Digits, CardTypes) ->
    case guess_type(Digits, CardTypes) of
	[] ->
	    unknown;
	[Name] ->
	    Name;
	Names ->
	    {error, Names}
    end.

type_test_() -> 
    [?_assertMatch(visa, type("4408041234567893")),
     ?_assertMatch(visa, type("4417123456789112")),
     ?_assertMatch({error, [acme, visa]}, type("4408041234567893", [card_type(acme, satisfy_all([]))|card_types()]))].
