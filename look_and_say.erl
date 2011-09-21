-module(look_and_say).
-export([look_and_say/1, main/0]).

look_and_say(L) ->
    look_and_say(L, []).

look_and_say([], Acc) ->
    lists:reverse(Acc);
look_and_say([X|XS], []) ->
    look_and_say(XS, [X,1]);
look_and_say([X|XS], [X,N|AS]) ->
    look_and_say(XS, [X,N + 1|AS]);
look_and_say([X|XS], Acc) ->
    look_and_say(XS, [X,1|Acc]).

main() ->
    io:format("~p~n", [look_and_say([])]),
    io:format("~p~n", [look_and_say([1])]),
    io:format("~p~n", [look_and_say([1,1])]),
    io:format("~p~n", [look_and_say([2,1])]),
    io:format("~p~n", [look_and_say([1,2,1,1])]),
    io:format("~p~n", [look_and_say([1,1,1,2,2,1])]).
