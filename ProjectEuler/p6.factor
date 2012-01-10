USING: math sequences kernel io present ;
IN: p6

: sum-of-squares ( seq -- n )
    [ sq ] map sum ;

: square-of-sums ( seq -- n )
    sum sq ;

: solve ( x -- d )
    iota [ 1 + ] map [ sum-of-squares ] [ square-of-sums ] bi - abs ;

100 solve present print