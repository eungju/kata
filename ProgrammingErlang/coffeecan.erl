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
    MAX_W = lists:min([W, 2]),
    MAX_B = lists:min([B, 2]),
    MIN_W = 2 - MAX_B,
    lists:nth(select(lists:seq(MIN_W, MAX_W)) + 1, [{black, black}, {black, white}, {white, white}]).

select(L) ->
    lists:nth(random:uniform(length(L)), L).
