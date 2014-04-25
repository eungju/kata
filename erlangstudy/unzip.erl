-module(unzip).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

unzip([], Unzipped, _State) ->
	lists:reverse(Unzipped);
unzip([$(|L], Unzipped, out_paran) ->
	unzip(L, "(" ++ Unzipped, in_paran);
unzip([$)|L], Unzipped, in_paran) ->
	unzip(L, ")" ++ Unzipped, out_paran);
unzip([$;|L], Unzipped, out_paran) ->
	unzip(L, "\n;" ++ Unzipped, out_paran);
unzip([$;|L], Unzipped,State) ->
	unzip(L, ";" ++ Unzipped, State);
unzip([H|L], Unzipped,State) ->
	unzip(L, [H|Unzipped], State).


%unzip([H|L], Unzipped) when H=:=$( ->

unzip(S) ->
	unzip(S, [], out_paran).

unzip_test_() -> [
	?_assertMatch( "1+2;\n3+4;\n", unzip( "1+2;3+4;" ) ),
	?_assertMatch( "1+2;\n5+6;\n", unzip( "1+2;5+6;" ) ),
	?_assertMatch( "for(i=0;i<10;i++)", unzip("for(i=0;i<10;i++)" ) ),
	?_assertMatch( "1+2;\n\"\n\";\n", unzip( "1+2;\"\n\";" ) ),
	?_assertMatch( "1+2;\n\";\n\";\n", unzip( "1+2;\";\n\";" ) )
].
