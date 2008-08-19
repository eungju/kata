-module(coffeecan).
-export([last_bean/2]).
-compile([export_all]).

last_bean(1, 0) ->
    white;
last_bean(0, 1) ->
    black;
last_bean(W, B) ->
    %io:format("~p, ~p~n", [W, B]),
    case choose(W, B) of
	[white, white] ->
	    last_bean(W - 2, B + 1);
	[black, black] ->
	    last_bean(W, B - 2 + 1);
	_ ->
	    last_bean(W, B - 1)
    end.

choose(W, B) ->
    choose(W, B, 2, []).

choose(_W, _B, 0, Acc) ->
    Acc;
choose(W, B, N, Acc) ->
    X = random:uniform(W + B),
    if
	X =< W -> choose(W - 1, B, N - 1, [white|Acc]);
	true -> choose(W, B - 1, N - 1, [black|Acc])
    end.
