USING: io kernel locals math math.functions math.ranges present
sequences ;
IN: p7

: prime? ( n -- ? ) dup sqrt [1,b] rest [ dupd rem zero? not ] all? nip ;

: primes ( n -- seq ) V{ } 2 [ 2over length > ] [ dup prime? [ 2dup swap push ] when 1 + ] while drop nip ;
    
: nth-prime ( n -- p ) primes last ;

10001 nth-prime present print 