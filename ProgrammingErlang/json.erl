-module(json).
-include_lib("eunit/include/eunit.hrl").

parse_integer([C|S]) when $0 =< C, C =< $9 ->
    {A, SS} = parse_integer(S),
    {[C|A] , SS};
parse_integer(S) ->
    {"", S}.

parse_integer_test() ->
    parse_integer("0") =:= {"0", ""},
    parse_integer("10") =:= {"10", ""},
    parse_integer("234.") =:= {"234", "."}.
