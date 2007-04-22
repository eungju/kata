-module(randompair).
-export([pairing/1]).
-author([eungju, gyehong]).
-compile(export_all).

%% First
pick_pair([]) ->
    [];
pick_pair([A,B|T]) ->
    [{A, B}|pick_pair(T)].

perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L,
	      T <- perms(L -- [H])].

pair(L) ->
    All = perms(L),
    pick_pair(lists:nth(random:uniform(length(All)), All)).

%% Second
pick_random(L) ->
    A = lists:nth(random:uniform(length(L)), L),
    {A, L -- [A]}.

fastpair([]) ->
    [];
fastpair(L) when length(L) rem 2 =:= 0 ->
    {A, L1} = pick_random(L),
    {B, L2} = pick_random(L1),
    [list_to_tuple(lists:sort([A, B]))|fastpair(L2)].

pairing(L) ->
    lists:sort(fastpair(L)).
