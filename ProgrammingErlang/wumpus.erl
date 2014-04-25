-module(wumpus).
-include_lib("eunit/include/eunit.hrl").

make_map(Rooms) ->
    Rooms.

neighb_rooms(Current, Map) ->
    lists:nth(Current, Map).

neighb_rooms_test_() ->
    % Map: 1---2
    Map=make_map([[2], [1]]),
    [?_assertMatch([2],neighb_rooms(1,Map)),
     ?_assertMatch([1],neighb_rooms(2,Map))].

-define(CANT_MOVE, "Can't move to room ~B.").
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

move(Current, Dest, Map) ->
    case lists:member(Dest, neighb_rooms(Current, Map)) of
	true ->
	    {Dest,neighb_rooms(Dest,Map)};
	_ ->
	    {error, format(?CANT_MOVE,[Dest])}
    end.

tunnels_msg(NeighbRooms) ->
    "Tunnels lead to room " ++ join2(",", " or ", lists:map(fun integer_to_list/1, NeighbRooms)) ++ ".".

move_test_() ->
    Map=make_map([[2], [1]]),
    Current = 1,
    [?_assertMatch({2, [1]}, move(Current, 2, Map)),
     ?_assertMatch({error, "Can't move to room 1."}, move(Current, 1, Map))].

tunnels_msg_test_()->
    [?_assertMatch("Tunnels lead to room 2.",
		   tunnels_msg([2])),
     ?_assertMatch("Tunnels lead to room 2 or 3.",
		   tunnels_msg([2,3])),
     ?_assertMatch("Tunnels lead to room 2,3 or 4.",
		   tunnels_msg([2,3,4]))].

join(Sep,[H|T])->
    lists:foldl(fun(X,Acc)->
			Acc++Sep++X end,
		H,T).
join_test_()->
    [?_assertMatch("a,b,c", join(",",["a","b","c"])),
     ?_assertMatch("2,3,4", join(",",["2","3","4"]))].

join2(_Sep1, _Sep2, []) ->
    "";
join2(_Sep1, _Sep2, [H]) ->
    H;
join2(_Sep1, Sep2, [H1,H2]) ->
    H1 ++ Sep2 ++ H2;
join2(Sep1, Sep2, [H|T]) ->
    H ++ Sep1 ++ join2(Sep1, Sep2, T).

join2_test_()->
    [?_assertMatch("a",join2(","," or ",["a"])),
     ?_assertMatch("a or b",join2(","," or ",["a","b"])),
     ?_assertMatch("a,b or c",join2(","," or ",["a","b","c"]))].

valid_input(Input, ValidRooms) ->
    lists:member(Input, lists:map(fun integer_to_list/1,ValidRooms)).

valid_input_test_() ->
    [?_assertMatch(true, valid_input("1", [1,2,3])),
     ?_assertMatch(false, valid_input("a", [1,2,3])),
     ?_assertMatch(false, valid_input("4", [1,2,3]))].

dountil(Inc, Until, Not, Extra, Initial) ->
    R = Inc(Initial),
    case Until(R, Extra) of
	false ->
	    N = 
	    dountil(1, Until, Not, Extra, R);
	true ->
	    R
    end.

dountil_DO_AND_UNTIL_test_()->
    Inc=fun(LastDo) ->
		LastDo+1 end,
    Until=fun(Do,Extra) ->
		  Do==3 end,
    Not=fun(Do,Extra) ->
		Do end,
    Extra=void,
    Initial=0,
    [?_assertMatch(3, dountil(Inc,Until,Not,Extra,Initial))].

dountil_NOT_test_()->
    Inc=fun(LastDo) ->
		LastDo+1 end,
    Until=fun(Do,Extra) ->
		  Do==-1 end,
    Not=fun(Do,Extra) ->
		-2 end,
    Extra=void,
    Initial=0,
    [?_assertMatch(-1, dountil(Inc,Until,Not,Extra,Initial))].
    
