-module(pascal).
-compile(export_all).

%% next(S) ->
%%     next(S, [1]).
%% next([_], Acc) ->
%%     [1|Acc];
%% next([A,B|L], Acc) ->
%%     next([B|L], [A+B|Acc]).

%% triangle(N) ->
%%     triangle(N, [[1]]).
%% triangle(1, Acc) ->
%%     lists:reverse(Acc); 
%% triangle(N, [S|L]) ->
%%     triangle(N - 1, [next(S),S|L]).

next(S) ->
    lists:foldl(fun(A,[B|L]) ->
			[A,A+B|L]
		end, [0], S).

triangle(N) ->
    lists:reverse(lists:foldl(fun(_,[S|_]=Acc) ->
 				      [next(S)|Acc]
 			      end, [[1]], lists:seq(1, N))).

