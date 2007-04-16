-module(baseball).
-export([make_game/0]).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% Possible counts
%% S3B0, S2B0, S1B2, S1B1, S1B0, S0B3, S0B2, S0B1, S0B0

%S3B0
ball_count([A, B, C], [A, B, C]) ->
    {{strike, 3}, {ball, 0}};
%S1B2
ball_count([A, B, C], [A, C, B]) ->
    {{strike, 1}, {ball, 2}};
ball_count([A, B, C], [C, B, A]) ->
    {{strike, 1}, {ball, 2}};
ball_count([A, B, C], [B, A, C]) ->
    {{strike, 1}, {ball, 2}};
%S0B3
ball_count([A, B, C], [B, C, A]) ->
    {{strike, 0}, {ball, 3}};
ball_count([A, B, C], [C, A, B]) ->
    {{strike, 0}, {ball, 3}};
ball_count([A, B, C], [B, A, C]) ->
    {{strike, 0}, {ball, 3}};
%S2B0
ball_count([A, B, _], [A, B, _]) ->
    {{strike, 2}, {ball, 0}};
ball_count([A, _, C], [A, _, C]) ->
    {{strike, 2}, {ball, 0}};
ball_count([_, B, C], [_, B, C]) ->
    {{strike, 2}, {ball, 0}};
%S1B1
ball_count([A, B, _], [A, _, B]) ->
    {{strike, 1}, {ball, 1}};
ball_count([A, B, _], [_, B, A]) ->
    {{strike, 1}, {ball, 1}};
ball_count([_, B, C], [C, B, _]) ->
    {{strike, 1}, {ball, 1}};
ball_count([_, B, C], [B, _, C]) ->
    {{strike, 1}, {ball, 1}};
ball_count([A, _, C], [A, C, _]) ->
    {{strike, 1}, {ball, 1}};
ball_count([A, _, C], [_, A, C]) ->
    {{strike, 1}, {ball, 1}};
%S0B2
ball_count([A, B, _], [_, A, B]) ->
    {{strike, 0}, {ball, 2}};
ball_count([A, B, _], [B, _, A]) ->
    {{strike, 0}, {ball, 2}};
ball_count([A, B, _], [B, A, _]) ->
    {{strike, 0}, {ball, 2}};
ball_count([_, B, C], [C, _, B]) ->
    {{strike, 0}, {ball, 2}};
ball_count([_, B, C], [B, C, _]) ->
    {{strike, 0}, {ball, 2}};
ball_count([_, B, C], [_, C, B]) ->
    {{strike, 0}, {ball, 2}};
ball_count([A, _, C], [C, A, _]) ->
    {{strike, 0}, {ball, 2}};
ball_count([A, _, C], [_, C, A]) ->
    {{strike, 0}, {ball, 2}};
ball_count([A, _, C], [C, _, A]) ->
    {{strike, 0}, {ball, 2}};
%S1B0
ball_count([A, _, _], [A, _, _]) ->
    {{strike, 1}, {ball, 0}};
ball_count([_, B, _], [_, B, _]) ->
    {{strike, 1}, {ball, 0}};
ball_count([_, _, C], [_, _, C]) ->
    {{strike, 1}, {ball, 0}};
%S0B1
ball_count([A, _, _], [_, A, _]) ->
    {{strike, 0}, {ball, 1}};
ball_count([A, _, _], [_, _, A]) ->
    {{strike, 0}, {ball, 1}};
ball_count([_, B, _], [B, _, _]) ->
    {{strike, 0}, {ball, 1}};
ball_count([_, B, _], [_, _, B]) ->
    {{strike, 0}, {ball, 1}};
ball_count([_, _, C], [_, C, _]) ->
    {{strike, 0}, {ball, 1}};
ball_count([_, _, C], [C, _, _]) ->
    {{strike, 0}, {ball, 1}};
%S0B0
ball_count([_, _, _], [_, _, _]) ->
    {{strike, 0}, {ball, 0}}.

dice([A, B, C]) ->
    [A, B, C];
dice(Selected) ->
    N = random:uniform(9),
    Exists = lists:member(N, Selected),
    if
	Exists ->
	    dice(Selected);
	true ->
	    dice([N|Selected])
    end.

make_game() ->
    Actual = dice([]),
    fun(Guess) ->
	    ball_count(Actual, Guess)
    end.

ball_count_test_() ->
    [?_assertMatch({{strike, 3}, {ball, 0}}, ball_count([1, 2, 3], [1, 2, 3])),
    ?_assertMatch({{strike, 2}, {ball, 0}}, ball_count([1, 2, 3], [1, 2, 4])),
    ?_assertMatch({{strike, 1}, {ball, 2}}, ball_count([1, 2, 3], [1, 3, 2])),
    ?_assertMatch({{strike, 1}, {ball, 1}}, ball_count([1, 2, 3], [3, 2, 7])),
    ?_assertMatch({{strike, 0}, {ball, 3}}, ball_count([1, 2, 3], [3, 1, 2])),
    ?_assertMatch({{strike, 0}, {ball, 2}}, ball_count([1, 2, 3], [2, 3, 5])),
    ?_assertMatch({{strike, 1}, {ball, 0}}, ball_count([1, 2, 3], [1, 4, 5])),
    ?_assertMatch({{strike, 0}, {ball, 1}}, ball_count([1, 2, 3], [3, 4, 8])),
    ?_assertMatch({{strike, 0}, {ball, 0}}, ball_count([1, 2, 3], [4, 5, 6]))].
