import math

def primeNumbers( k ):
    primes = []
    i = 2;
    #while len( primes ) < k:
    while not primes or primes[ -1 ] < k:
        if isPrimeNumber( i ):
            primes.append( i )
        i = i + 1
    return primes

def isPrimeNumber( n ):
    for i in range( 2, math.sqrt( n ) ):
        if ( n % i ) == 0:
            return 0
    return 1

print primeNumbers(50000)
