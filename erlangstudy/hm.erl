-module(hm).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

findall(L,C)->
    findall(L,C,1, fun(X)->X end).

findall([], C, Seq, Co) ->
    Co([]);
findall([H|T], C, Seq,Co) when H == C -> 
    findall(T,C,Seq+1, fun(X)-> Co([Seq|X]) end);
findall([H|T],C,Seq,Co) ->
    findall(T,C,Seq+1, fun(X)-> Co(X) end).

findall_test() ->
   ?assertMatch([3,4,6],findall("cheese",$e)).


