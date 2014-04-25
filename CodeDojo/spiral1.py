"""
03:30 ~ 05:15
"""

import math, sys
import unittest

class Spiral:
    def __init__( self, row, col ):
        self.rows = []
        for r in range( row ):
            self.rows.append( [ -1 ] * col )

        l = [ col, row - 1 ]
        i = 0
        r, c = 0, 0
        tc = 0
        while i < row * col:
            for j in range( l[ tc % 2 ] ):
                self.set( r, c, i )
                i = i  + 1
                if j == l[ tc % 2 ] - 1:
                    l[ tc % 2 ] = l[ tc % 2 ] - 1
                    tc = tc + 1
                r = r + int( math.sin( tc * math.pi / 2 ) )
                c = c + int( math.cos( tc * math.pi / 2 ) )

    def row( self, i ):
        return self.rows[ i ]
    
    def col( self, i ):
        return map( lambda x: x[ i ], self.rows )

    def set( self, r, c, v ):
        self.rows[ r ][ c ] = v

    def display( self ):
        fmt = '%%%dd' % len( str( len( self.rows ) * len( self.rows[ 0 ] ) ) )
        for r in self.rows:
            for v in r:
                print fmt % v,
            print
        
class SpiralTestCase( unittest.TestCase ):
    def test1xN( self ):
        spiral1x6 = Spiral( 1, 6 )
        self.assertEquals( [ 0, 1, 2, 3, 4, 5 ], spiral1x6.row( 0 ) )
        spiral1x9 = Spiral( 1, 9 )
        self.assertEquals( [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ], spiral1x9.row( 0 ) )

    def testNx1( self ):
        spiral6x1 = Spiral( 6, 1 )
        self.assertEquals( [ 0, 1, 2, 3, 4, 5 ], spiral6x1.col( 0 ) )
        spiral9x1 = Spiral( 9, 1 )
        self.assertEquals( [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ], spiral9x1.col( 0 ) )

    def test1x1( self ):
        spiral1x1 = Spiral( 1, 1 )
        self.assertEquals( [ 0 ], spiral1x1.row( 0 ) )
        self.assertEquals( [ 0 ], spiral1x1.col( 0 ) )

    def testNxN( self ):
        spiral6x6 = Spiral( 6, 6 )
        self.assertEquals( [ 0, 1, 2, 3, 4, 5 ], spiral6x6.row( 0 ) )
        self.assertEquals( [ 19, 20, 21, 22, 23, 6 ], spiral6x6.row( 1 ) )
        self.assertEquals( [ 18, 31, 32, 33, 24, 7 ], spiral6x6.row( 2 ) )
        self.assertEquals( [ 17, 30, 35, 34, 25, 8 ], spiral6x6.row( 3 ) )
        self.assertEquals( [ 16, 29, 28, 27, 26, 9 ], spiral6x6.row( 4 ) )
        self.assertEquals( [ 15, 14, 13, 12, 11, 10 ], spiral6x6.row( 5 ) )

#unittest.main()

row, col = sys.stdin.readline().split()
row, col = int( row ), int( col )
spiral = Spiral( row, col )
spiral.display()
