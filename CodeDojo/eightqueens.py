def isSafe( x, y ):
    return x[ 0 ] != y[ 0 ] and x[ 1 ] != y[ 1 ] and abs( y[ 0 ] - x[ 0 ] ) != abs( y[ 1 ] - x[ 1 ] )
    
def isSafeWith( queens, p ):
    for q in queens:
        if not isSafe( q, p ):
            return 0
    return 1

def solve( queens ):
    for j in range( 32 ):
        p = ( len(queens), j )
        if isSafeWith( queens, p ):
            solution = solve( queens + [ p ] )
            if 8 == len( solution ):
                return solution
    return queens

def validate( queens ):
    for i in queens:
        for j in queens:
            if i != j and not isSafe( i, j ):
                return 0
    return 1

#print solve( [] )           
#print validate(solve( [] ))
print solve( [] )
#print len( solutions )
