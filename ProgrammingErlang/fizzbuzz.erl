-module(fizzbuzz).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

game(N) -> game(1, N).

game(N, N) -> [fizzbuzz(N)];
game(I, N) -> [fizzbuzz(I) | game(I + 1, N)].

fizzbuzz(I) ->
    F = I rem 3, B = I rem 5,
    if
	F == 0, B == 0 -> fizzbuzz;
	F == 0 -> fizz;
	B == 0 -> buzz;
	true -> I
    end.

number_test() -> fizzbuzz(1) == 1.
fizz_test() -> fizzbuzz(3) == fizz, fizzbuzz(6) == fizz.
buzz_test() -> fizzbuzz(5) == buzz, fizzbuzz(10) == buzz.
fizzbuzz_test() -> fizzbuzz(15) == fizzbuzz, fizzbuzz(15) == fizzbuzz.

game_turns_test() -> length(game(1, 2)) == 1, length(game(1, 3)) == 3.

main(Turn) ->
    io:format("~w~n", [game(Turn)]).
