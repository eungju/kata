-module(baseball2).
-include_lib("eunit/include/eunit.hrl").
-export([make_game/0, make_game/1]).

is_strike(Answer, Nth, Bet) ->
    lists:nth(Nth, Answer) =:= Bet.

is_ball(Answer, Bet) ->
    lists:member(Bet, Answer).

pitch(Answer, Nth, Bet, {Strikes, Balls}) ->
    case {is_strike(Answer, Nth, Bet), is_ball(Answer, Bet)} of
	{true, _} ->
	    {Strikes + 1, Balls};
	{false, true} ->
	    {Strikes, Balls + 1};
	_ ->
	    {Strikes, Balls}
    end.

pitch_test_() ->
    [?_assertMatch({2, 1}, pitch([6, 7, 8], 1, 6, {1, 1})),
    ?_assertMatch({1, 2}, pitch([6, 7, 8], 2, 6, {1, 1})),
    ?_assertMatch({1, 1}, pitch([6, 7, 8], 2, 5, {1, 1}))].

ball_count(Answer, Guess) ->
    ball_count(Answer, Guess, 1, {0, 0}).

ball_count(_, [], _, BallCounts) ->
    BallCounts;
ball_count(Answer, [Bet|T], Nth, BallCounts) ->
    ball_count(Answer, T, Nth + 1, pitch(Answer, Nth, Bet, BallCounts)).
    
ball_count_test_() ->
    [?_assertMatch({0, 0}, ball_count([1, 2, 3], [9, 8, 7])),
     ?_assertMatch({3, 0}, ball_count([1, 2, 3], [1, 2, 3])),
     ?_assertMatch({0, 4}, ball_count([1, 2, 3, 4], [4, 3, 2, 1]))].

dice(Wanted) ->
    dice(Wanted, []).

dice(0, L) ->
    L;
dice(Wanted, Selected) ->
    N = random:uniform(9),
    Exists = lists:member(N, Selected),
    if
        Exists ->
            dice(Wanted, Selected);
        true ->
            dice(Wanted - 1, [N|Selected])
    end.

dice_test_() ->
    [?_assertMatch(3, length(dice(3))),
     ?_assertMatch(4, length(dice(4)))].

make_game() ->
    make_game(dice(3, [])).

make_game(Answer) ->
    fun(Guess) ->
	    ball_count(Answer, Guess) end.
