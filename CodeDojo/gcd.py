import unittest

def gcd1( x, y ):
    for i in range( 1, min( ( x, y ) ) + 1 ):
        if ( x % i == 0 ) and ( y % i == 0 ):
            n = i
    return n

def gcd2( x, y ):
    for i in range( min( ( x, y ) ), 0, -1  ):
        if ( x % i == 0 ) and ( y % i == 0 ):
            return i

gcd = gcd2

class GcdTestCase(unittest.TestCase):
    def testGcd( self ):
        self.assertEquals( 2, gcd( 2, 4 ) )
        self.assertEquals( 1, gcd( 3, 2 ) )
        self.assertEquals( 2, gcd( 206, 40 ) )

    def testSameNumber( self ):
        self.assertEquals( 4, gcd( 4, 4 ) )

    def testZero( self ):
        self.assertEquals( None, gcd( 0, 3 ) )
        self.assertEquals( None, gcd( 0, 0 ) )

unittest.main()
