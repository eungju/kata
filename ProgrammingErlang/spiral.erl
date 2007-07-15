-module(spiral).
-export([spiral/1, array/2, spiral_array/1, benchmark/0]).
-compile(export_all).

forward({X, Y}, east) ->
    {X + 1, Y};
forward({X, Y}, south) ->
    {X, Y + 1};
forward({X, Y}, west) ->
    {X - 1, Y};
forward({X, Y}, north) ->
    {X, Y - 1}.

%% engrave(Pos, N, Board) ->
%%     [{Pos, N}|Board].

%% engraved(Pos, Board) ->
%%     proplists:get_value(Pos, Board).

%% visited(Pos, Board) ->
%%     proplists:is_defined(Pos, Board).

%% empty() ->
%%     [].

engrave(Pos, N, Board) ->
    gb_trees:insert(Pos, N, Board).

engraved(Pos, Board) ->
    gb_trees:get(Pos, Board).
    
visited(Pos, Board) ->
    gb_trees:is_defined(Pos, Board).

empty() ->
    gb_trees:empty().

%% engrave(Pos, N, Board) ->
%%     dict:append(Pos, N, Board).

%% engraved(Pos, Board) ->
%%     dict:fetch(Pos, Board).
    
%% visited(Pos, Board) ->
%%     dict:is_key(Pos, Board).

%% empty() ->
%%     dict:new().

is_inside({X, Y}, Size) ->
    (0 < X) and (X =< Size) and (0 < Y) and (Y =< Size).

need_to_turn(Pos, Dir, Size, Board) ->
    Front = forward(Pos, Dir),
    not is_inside(Front, Size) or visited(Front, Board).

turn_right(east) -> south;
turn_right(south) -> west;
turn_right(west) -> north;
turn_right(north) -> east.
    
walk(Size, N, _Pos, _Dir, Board) when N > Size * Size ->
    Board;
walk(Size, N, Pos, Dir, Board) ->
    NextDir = case need_to_turn(Pos, Dir, Size, Board) of
		  true -> 
		      turn_right(Dir);
		  false ->
		      Dir
	      end,
    walk(Size, N + 1, forward(Pos, NextDir), NextDir, engrave(Pos, N, Board)).

spiral(Size) ->
    walk(Size, 1, {1, 1}, east, empty()).

row(0, _Y, _Board, Acc) ->
    Acc;
row(X, Y, Board, Acc) ->
    row(X - 1, Y, Board, [engraved({X, Y}, Board)|Acc]).
row(Size, Y, Board) ->
    row(Size, Y, Board, []).

array(_Size, _Board, 0, Acc) ->
    Acc;
array(Size, Board, Y, Acc) ->
    array(Size, Board, Y - 1, [row(Size, Y, Board)|Acc]).
array(Size, Board) ->
    array(Size, Board, Size, []).

spiral_array(Size) ->
    array(Size, spiral(Size)).

benchmark() ->
    Size = 1000,
    {SpiralTime, Spiral} = timer:tc(?MODULE, spiral, [Size]),
    {ArrayTime, _Array} = timer:tc(?MODULE, array, [Size, Spiral]),
    io:format("spiral: ~p, array: ~p~n",
	      [SpiralTime / 1000000, ArrayTime / 1000000]).

profile() ->
    Size = 100,
    cprof:start(),
    spiral_array(Size),
    cprof:pause(),
    io:format("~p~n", [cprof:analyse(?MODULE)]),
    cprof:stop(),
    fprof:apply(?MODULE, spiral_array, [Size]),
    fprof:profile(),
    fprof:analyse([{dest, ""}]).
