-module(lovestory).
-export([start/0]).

me(Her) ->
    receive
	{meet, you, PhoneNumber} ->
	    PhoneNumber ! {self(), "I love you."},
	    me(PhoneNumber);
	{Her, "I love you."} ->
	    io:format("She said to me 'I love you'~n"),
	    Her ! {self(), "I love you."},
	    me(Her);
	_ ->
	    io:format("It doesn't matter.~n"),
	    me(Her)
    end.

you(Him) ->
    receive
	{meet, me, PhoneNumber} ->
	    you(PhoneNumber);
	{Him, "I love you."} ->
	    io:format("He said to me 'I love you'~n"),
	    Him ! {self(), "I love you."},
	    you(Him);
	_ ->
	    io:format("It doesn't matter.~n"),
	    you(Him)
    end.

start() ->
    Me = spawn(fun() -> me(void) end),
    You = spawn(fun() -> you(void) end),
    Me ! {meet, you, You},
    You ! {meet, me, Me},
    World = fun(NextWorld) ->
		   case random:uniform(2) of
		       1 ->
			   You ! disturb;
		       2 ->
			   Me ! disturb
		   end,
		   receive
		   after 1000 ->
			   void
		   end,
		   NextWorld(NextWorld)
	   end,
    World(World).
