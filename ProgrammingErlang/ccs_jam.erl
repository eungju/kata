-module(ccs_jam).
-export([seed/0, join/1]).

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
    Seed = spawn(fun() -> loop() end),
    pg2:create(ccs_jam),
    pg2:join(ccs_jam, Seed),
    Seed ! ".".

join(Node) ->    
    net_kernel:connect_node(Node),
    NewMember = spawn(fun() -> loop() end),
    case pg2:join(ccs_jam, NewMember) of
	{error, _} ->
	    timer:sleep(1000),
	    pg2:join(ccs_jam, NewMember);
	ok ->
	    ok
    end.

loop() ->
    receive
	Word ->
	    NextWord = next_word(Word),
	    io:format("\e[~wm~s ", [29 + random:uniform(10), NextWord]),
	    timer:sleep(100),
	    Members = pg2:get_members(ccs_jam),
	    lists:nth(random:uniform(length(Members)), Members) ! NextWord,
	    loop()
    end.
