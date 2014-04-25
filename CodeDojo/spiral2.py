import math

class SpiralWalker:
    def __init__( self, width, height ):
	self.width = width
	self.height = height
	self.current = 0
	self.x = 0
	self.y = 0
	self.turn = 0

	self.rows = []
	for i in range( self.height ):
	    self.rows.append( [ None ] * width )

	while self.current < self.width * self.height:
	    print self.x, self.y, self.current
	    self.step()

    def step( self ):
	self.mark()
	if self.isCorner():
	    self.turn = self.turn + 1
	    print "TURN"
	self.y = self.y + int( math.sin( self.turn * math.pi / 2 ) )
	self.x = self.x + int( math.cos( self.turn * math.pi / 2 ) )
	self.current = self.current + 1

    def mark( self ):
	self.rows[ self.y ][ self.x ] = self.current

    def isCorner( self ):
	nextY = self.y + int( math.sin( self.turn * math.pi / 2 ) )
	nextX = self.x + int( math.cos( self.turn * math.pi / 2 ) )
	return nextY not in range( self.height ) or nextX not in range( self.width ) or self.isMarked( nextX, nextY )

    def isMarked( self, x, y ):
	return self.rows[ y ][ x ] != None

    def display( self ):
	for row in self.rows:
	    for i in row:
		print " %2d" % i,
	    print

walker = SpiralWalker( 6, 6 )
walker.display()
