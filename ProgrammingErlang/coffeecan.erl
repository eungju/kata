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
	{white, white} ->
	    last_bean(W - 2, B + 1);
	{black, black} ->
	    last_bean(W, B - 2 + 1);
	_ ->
	    last_bean(W, B - 1)
    end.

choose(W, B) ->
    S0 = lists:concat([lists:duplicate(W, white), lists:duplicate(B, black)]),
    P1 = lists:nth(random:uniform(length(S0)), S0),
    S1 = lists:delete(P1, S0),
    P2 = lists:nth(random:uniform(length(S1)), S1),
    {P1, P2}.
