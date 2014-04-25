-module(isbn).
-include_lib("eunit/include/eunit.hrl").

isbn10_to_isbn13(Isbn10) ->
    "978" + Isbn10.

isbn10_to_isbn13_test() ->
    ?assertMatch("9780940016736", isbn10_to_isbn13("0940016737")).
