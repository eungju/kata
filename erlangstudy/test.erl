-module(test).
-export([test/0]).

test() ->
  A = 1,
  C = [X || X <- [1,2,5]],
  io:format("~p",[C]).
