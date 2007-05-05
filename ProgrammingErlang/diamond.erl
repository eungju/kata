-module(diamond).
-include_lib("eunit/include/eunit.hrl").
-export([draw/1, draw_on_console/1]).

odds(N) ->
    odds(N * 2 - 1, []).

odds(1, Acc) ->
    [1|Acc];
odds(I, Acc) ->
    odds(I - 2, [I|Acc]).

odds_test_() ->
    [?_assertMatch([1], odds(1)),
     ?_assertMatch([1,3], odds(2))].

plan(N) ->
    Top = odds(N),
    [Width|Bottom] = lists:reverse(Top),
    {Width, Top ++ Bottom}.

plan_test_() ->
    [?_assertMatch({3, [1,3,1]}, plan(2)),
     ?_assertMatch({5, [1,3,5,3,1]}, plan(3))].

draw_line(N, W) ->
    Space = lists:duplicate((W - N) div 2, $ ),
    Space ++ lists:duplicate(N, $*) ++ Space.

draw_line_test_() ->
    [?_assertMatch("*", draw_line(1, 1)),
     ?_assertMatch(" * ", draw_line(1, 3)),
     ?_assertMatch("  ***  ", draw_line(3, 7))].

draw(Size) ->
    {Width, Design} = plan(Size),
    lists:map(fun(N) -> draw_line(N, Width) end, Design).

draw_on_console(Size) ->
    lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, draw(Size)).
