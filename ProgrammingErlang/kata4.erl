-module(kata4).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

weather(Line) ->
    [Name,F1,F2|_] = string:tokens(Line," "),
    [V1,V2] = lists:map(fun(X) ->
				element(1,string:to_integer(X))
			end, [F1,F2]),
    {Name,V1,V2}.

weather_test_() ->
    [?_assertMatch({"1",88,59}, weather("   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5")),
     ?_assertMatch({"1",88,59}, weather("   1  88*    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5")),
     ?_assertMatch({"1",88,59}, weather("   1  88    59*    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5"))].

is_weather(Line) ->
    Tokens = string:tokens(Line, " "),
    if
	length(Tokens) > 0 ->
	    {error,no_integer} =/= string:to_integer(lists:nth(1,Tokens));
	true ->
	    false
    end.

is_weather_test_() ->
    [?_assertMatch(true, is_weather("   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5")),
     ?_assertMatch(false, is_weather("")),
     ?_assertMatch(false, is_weather(" MMU June 2002")),
     ?_assertMatch(false, is_weather("  Dy MxT   MnT   AvT   HDDay  AvDP 1HrP TPcpn WxType PDir AvSp Dir MxS SkyC MxR MnR AvSLP"))].

min_var([H|T]) ->
    min_var(T,H).
min_var([], Min) ->
    Min;
min_var([{_,V1,V2}=H|T], {_,MV1,MV2}=Min) ->
    if
 	abs(V1 - V2) < abs(MV1 - MV2) ->
 	    min_var(T,H);
 	true ->
 	    min_var(T,Min)
    end.

min_var_test_() ->
    [?_assertMatch({1,1,1}, min_var([{1,1,1}])),
     ?_assertMatch({2,1,1}, min_var([{1,3,4},{2,1,1}]))].

read_lines(FileName) ->
    {ok, S} = file:open(FileName, read),
    Lines = read_lines(S, []),
    file:close(S),
    Lines.

read_lines(S, Acc) ->
    case io:get_line(S, '') of
	eof ->
	    lists:reverse(Acc);
	Line ->
	    read_lines(S, [Line|Acc])
    end.
    
weather_main() ->    
    main(fun weather/1, fun is_weather/1, "weather.dat").

team(Line) ->
    Tokens = string:tokens(Line," "),
    Name = lists:nth(2, Tokens),
    F1 = lists:nth(7, Tokens),
    F2 = lists:nth(9, Tokens),
    [V1,V2] = lists:map(fun list_to_integer/1, [F1,F2]),
    {Name,V1,V2}.

is_team(Line) ->
    Tokens = string:tokens(Line, " "),
    if
	length(Tokens) > 0 ->
	    {error,no_integer} =/= string:to_integer(lists:nth(1,Tokens));
	true ->
	    false
    end.

football_main() ->    
    main(fun team/1, fun is_team/1, "football.dat").

main(Parse, Predicate, FileName) ->    
    {Name,_,_} = min_var(lists:map(Parse, lists:filter(Predicate, read_lines(FileName)))),
    io:format("~p~n", [Name]).
