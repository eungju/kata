-module(spiral2).
-export([spiral_array/1, benchmark/0]).
-include_lib("eunit/include/eunit.hrl").

edge_cells(1) ->
    1;
edge_cells(Size) ->
    4 * (Size - 1).

edge_cells_test_() ->
    [?_assertMatch(1, edge_cells(1)),
     ?_assertMatch(4, edge_cells(2)),
     ?_assertMatch(8, edge_cells(3)),
     ?_assertMatch(12, edge_cells(4)),
     ?_assertMatch(16, edge_cells(5))].

spiral_length(0, _Size, Length) ->
    Length;
spiral_length(Nth, Size, Length) ->
    spiral_length(Nth - 1, Size - 2, edge_cells(Size) + Length).
spiral_length(Nth, Size) ->
    spiral_length(Nth, Size, 0).

spiral_length_test_() ->
    [?_assertMatch(1, spiral_length(1, 1)),
     ?_assertMatch(16, spiral_length(1, 5)),
     ?_assertMatch(24, spiral_length(2, 5)),
     ?_assertMatch(4, spiral_length(1, 2)),
     ?_assertMatch(9, spiral_length(2, 3))].

shell({X, Y}, Size) ->
    lists:min([X, Size - X + 1, Y, Size - Y + 1]).

shell_test_() ->
    [?_assertMatch(1, shell({1,1}, 5)),
     ?_assertMatch(1, shell({5,5}, 5)),
     ?_assertMatch(3, shell({3,3}, 5)),
     ?_assertMatch(1, shell({2, 1}, 5))].

n({X, Y}, Size) ->
    Nth = shell({X,Y}, Size),
    if
	X >= Y ->
	    spiral_length(Nth - 1, Size) + (X - 1 - (Nth - 1)) + (Y - 1 - (Nth - 1)) + 1;
	true ->
	    spiral_length(Nth, Size) - (X - 1 - (Nth - 1)) - (Y - 1 - (Nth - 1)) + 1
    end.

n_test_() ->
    [?_assertMatch(1, n({1,1}, 5)),
     ?_assertMatch(16, n({1,2}, 5)),
     ?_assertMatch(15, n({1,3}, 5)),
     ?_assertMatch(17, n({2,2}, 5)),
     ?_assertMatch(2, n({2,1}, 5))].

row(_Size, 0, _Y, Acc) ->
    Acc;
row(Size, X, Y, Acc) ->
    row(Size, X - 1, Y, [n({X, Y}, Size)|Acc]).
row(Size, Y) ->
    row(Size, Size, Y, []).

array(_Size, 0, Acc) ->
    Acc;
array(Size, Y, Acc) ->
    array(Size, Y - 1, [row(Size, Y)|Acc]).
array(Size) ->
    array(Size, Size, []).

spiral_array(Size) ->
    array(Size).

benchmark() ->
    Size = 1000,
    {SpiralArrayTime, _} = timer:tc(?MODULE, spiral_array, [Size]),
    io:format("spiral_array: ~p~n", [SpiralArrayTime / 1000000]).
