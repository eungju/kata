-module(threenpo).
-export([cycle/1, maxcycle/2, maxstep/2]).
-compile(export_all).

%%first
step(N) -> step(N, 0).
step(1, S) -> S + 1;
step(N, S) when N rem 2 =:= 0 -> step(N div 2, S + 1);
step(N, S) -> step((3 * N + 1) div 2, S+2).

maxstep(A, B) ->
    lists:max(lists:map(fun step/1, lists:seq(A, B))).
  
%%second
%% cycle(N) -> cycle(N, N, 0).
%% cycle(N, M, S) when N =:= 1 -> S + 1;
%% cycle(N, M, S) ->
%%     SN = case get(N) of
%%         undefined when N rem 2 =:= 0 ->
%%             cycle(N div 2, M, S + 1);
%%         undefined ->
%%             cycle(3 * N + 1, M, S + 1);
%%         Cached -> S + Cached
%%     end,
%%     put(M, SN),
%%     SN.
 

%%third
cycle(1) ->
    1;
cycle(N) ->
    case get(N) of
    undefined ->
        SN = if
             N rem 2 =:= 0 ->
             1 + cycle(N div 2);
             true ->
             2 + cycle((3 * N + 1) div 2)
         end,
        put(N, SN),
        SN;
    Cached ->
        Cached
    end.

maxcycle(A, B) ->
    lists:max(lists:map(fun cycle/1, lists:seq(A, B))).
