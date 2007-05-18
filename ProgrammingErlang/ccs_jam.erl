-module(ccs_jam).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, seed/0, join/1, loop/1]).

next_word(Word) ->
    case Word of
	"Code" ->
	    "can";
	"can" ->
	    "be";
	"be" ->
	    "an";
	"an" ->
	    "art";
	"art" ->
	    ".";
	"." ->
	    "Code"
    end.
	     
seed() ->
    Seed = spawn(fun() -> loop(undefined) end),
    Seed ! {join, undefined, Seed},
    Seed ! ".",
    Seed.

join(Friend) ->    
    NewMember = spawn(fun() -> loop(Friend) end),
    Friend ! {join, Friend, NewMember},
    NewMember.

loop(Neighbor) ->
    receive
	{join, Friend, NewMember} when Friend =:= Neighbor ->
	    io:format("New member ~p has joined.~n", [NewMember]),
	    loop(NewMember);
	{join, Friend, NewMember} ->
	    io:format("~p:, Friend: ~p, NewMember: ~p~n", [self(), Friend, NewMember]),
	    Neighbor ! {join, Friend, NewMember}, 
	    loop(Neighbor);
	{stop} ->
	    void;
	Word ->
	    NextWord = next_word(Word),
	    io:format("~s ", [NextWord]),
	    timer:sleep(500),
	    Neighbor ! NextWord,
	    loop(Neighbor)
    end.

make_group(1) ->
    seed();
make_group(N) ->
    join(make_group(N - 1)).

main() ->
    make_group(10).

