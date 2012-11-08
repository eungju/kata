USING: accessors arrays combinators command-line formatting fry hash-sets io
io.encodings.utf8 io.files kernel math math.parser math.vectors namespaces
peg.ebnf prettyprint sequences sets splitting syntax tools.test ;
IN: round2

{ t } [ f not ] unit-test

EBNF: parse-game-spec
digit=([0-9]) => [[ digit> ]]
number=(digit)+ => [[ 10 digits>integer ]]
game-spec=(number " "~ number) " "~ number " "~ (number " "~ number) " "~ ("B"~ digit+ ("/S"~ digit+)?)
;EBNF

{ V{ V{ 1 2 } 3 V{ 4 5 } V{ V{ 2 } V{ 2 3 } } } } [ "1 2 3 4 5 B2/S23" parse-game-spec ] unit-test
{ V{ V{ 1 2 } 3 V{ 4 5 } V{ V{ 2 } f } } } [ "1 2 3 4 5 B2" parse-game-spec ] unit-test

TUPLE: game size costs rule generation ;
    
: <game> ( size costs rule seeds -- game )
    game boa ;

: with-game ( game quot -- )
    game swap with-variable ; inline

: neighbors ( pos size -- set )
    [ { -1 0 1 } dup [ 2array over v+ ] cartesian-map concat remove ] dip
    '[ _ v< vall? ] [ { 0 0 } v>= vall? ] [ bi and ] 2curry filter ;

{ { { 0 1 } { 0 2 } { 0 3 }
    { 1 1 } { 1 3 }
    { 2 1 } { 2 2 } { 2 3 } }
} [ { 1 2 } { 5 5 } neighbors ] unit-test

{ { { 0 1 }
    { 1 0 } { 1 1 } }
} [ { 0 0 } { 5 5 } neighbors ] unit-test

{ { { 3 3 } { 3 4 }
    { 4 3 } }
} [ { 4 4 } { 5 5 } neighbors ] unit-test

SYMBOL: op-set
SYMBOL: op-next

: naive-solution ( game -- plan )
    generation>> [ op-set swap 2array ] map ;

: birth-subjects ( game -- subjects )
    [ generation>> ] [ size>> ] bi dupd '[ _ neighbors ] [ union ] map-reduce swap diff ;

{ HS{ { 0 1 } { 0 2 } { 1 0 } { 1 2 } { 2 0 } { 2 1 } { 2 2 } } }
[ { 5 5 } f f { { 0 0 } { 1 1 } } <game> birth-subjects >hash-set ] unit-test

: print-plan ( plan -- )
    [ length pprint nl ] [
        [ dup first
          {
              { op-set [ second first2 "SET %d %d" printf nl ] }
              { op-next [ drop "NEXT" print ] }
          } case
        ] each
    ] bi ;

: main ( -- )
    (command-line) second utf8 [
        readln string>number [
            readln parse-game-spec
            dup second [ readln " " split [ string>number ] map ] replicate
            [ 1 swap remove-nth first3 ] dip <game>
            naive-solution print-plan
        ] times
    ] with-file-reader ;

MAIN: main
