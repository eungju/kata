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
    choose(lists:concat([lists:duplicate(W, white), lists:duplicate(B, black)]),
 2, []).

choose(_S, 0, Acc) ->
    Acc;
choose(S, N, Acc) ->
    P = lists:nth(random:uniform(length(S)), S),
    choose(lists:delete(P, S), N - 1, [P|Acc]).
