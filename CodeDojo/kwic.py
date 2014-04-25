class LineReader:
    def __init__( self, file ):
        self._file = file
        self._nextLine = self._file.readline()

    def hasNext( self ):
        return len( self._nextLine )

    def next( self ):
        next = self._nextLine
        self._nextLine = self._file.readline()
        return next

class IndexStore:
    def __init__( self ):
        self.indexes = []

    def addLine( self, line ):
        shifter = LineShifter( line )
        for index in shifter.indexes():
            self.indexes.append( index )
        self.indexes.sort()

    def visit( self, visitor ):
        for index in self.indexes:
            visitor.doVisit( index )

class LineShifter:
    def __init__( self, line ):
        self._line = line

    def indexes( self ):
        indexes = []
        words = self._line.split()
        
        indexes.append( words[:] )
        for i in range( len( words ) - 1 ):
            last = words.pop()
            words.insert( 0, last )
            indexes.append( words[:] )
            
        return indexes
    
class ShiftedLine:
    def __init__( self, words ):
        self._words = words

    def __str__( self ):
        return ' '.join( words )

class PrintVisitor:
    def doVisit( self, index ):
        print index

if __name__ == '__main__':
    from StringIO import StringIO
    
    store = IndexStore()
    
    reader = LineReader( StringIO( '3 5 4 1 2\nA B C D E F G H\nzact\nthe pragmatic programmer\n' ) )
    while reader.hasNext():
        line = reader.next()
        store.addLine( line )

    visitor = PrintVisitor()
    store.visit( visitor )
