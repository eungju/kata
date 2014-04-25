from bisect import insort

class Simulator:
    def __init__( self ):
        self.clock = Clock()
        self.queue = EventQueue()
    
    def run( self ):
        while not self.queue.isEmpty():
            event = self.queue.pop()
            self.clock.time = event.time
            event.process( self.queue )
            self.log( event )

    def log( self ):
        pass

class Clock:
    def __init__( self ):
        self.time = 0
        
class Event:
    def __init__( self, time, *args ):
        self.name = str( self.__class__ ).split( '.' )[ -1 ][ : -len( 'Event' ) ]
        self.time = time
        self._args = args

    def __cmp__( self, event ):
        return cmp( self.time, event.time )

class EventQueue:
    def __init__( self ):
        self.events = []
        
    def push( self, event ):
        insort( self.events, event )
        
    def pop( self ):
        return self.events.pop( 0 )

    def isEmpty( self ):
        return 0 == len( self.events )


class ElevatorSimulator( Simulator ):
    def __init__( self, stages, elevators ):
        Simulator.__init__( self )

        self.stages = []
        for i in range( stages ):
            self.stages.append( Stage( i + 1 ) )
        for i in range( len( self.stages ) ):
            if 0 != i:
                self.stages[ i ].prev = self.stages[ i - 1 ]
            if len( self.stages ) - 1 != i:
                self.stages[ i ].next = self.stages[ i + 1 ]
            
        self.elevators = []
        for i in range( elevators ):
            self.elevators.append( Elevator() )

    def log( self, event ):
        print '%s:%d: ' % ( event.name, event.time ),
        for s in self.stages:
            print 'S%s(%d, %d)' % ( s.index, len( s.waitingPeoples ), len( s.workingPeoples ) ),
        print
 
class People:
    def __init__( self, nodes ):
        self.nodes = nodes
        self.currentNode = 0;

    def next( self ):
        self.currentNode = self.currentNode + 1
        return self.nodes[ self.currentNode ]

    def currentStage( self ):
        return self.nodes[ self.currentNode ][ 0 ]

    def currentWorkTime( self ):
        return self.nodes[ self.currentNode ][ 1 ]

    def nextStage( self ):
        return self.nodes[ self.currentNode + 1 ][ 0 ]

class Stage:
    def __init__( self, index ):
        self.index = index
        self.prev = None
        self.next = None
        self.waitingPeoples = []
        self.workingPeoples = []

    def waitingMoveUp( self ):
        peoples = []
        for p in self.waitingPeoples:
            if self.index < p.nextStage():
                peoples.append( p )

    def waitingMoveDown( self ):
        peoples = []
        for p in self.waitingPeoples:
            if self.index > p.nextStage():
                peoples.append( p )

class Elevator:
    def __init__( self ):
        self.peoples = []
        self.direction = None

    def isFull( self ):
        return len( self.peoples ) >= 11

    def isMoveUp( self ):
        return self.direction == 'UP'

    def isMoveDown( self ):
        return self.direction == 'DOWN'

class OutEvent( Event ):
    def process( self, queue ):
        stage = self._args[ 0 ]
        people = self._args[ 1 ]
        stage.workingPeoples.remove( people )

class InEvent( Event ):
    def process( self, queue ):
        stage = self._args[ 0 ]
        people = self._args[ 1 ]
        stage.workingPeoples.append( people )
        queue.push( WaitElevatorEvent( self.time + people.currentWorkTime(), stage, people ) )

class WaitElevatorEvent( Event ):
    def process( self, queue ):
        stage = self._args[ 0 ]
        people = self._args[ 1 ]
        stage.workingPeoples.remove( people )
        stage.waitingPeoples.append( people )
        queue.push( MoveElevatorEvent( self.time + 5, stage.next ) )

class MoveElevatorEvent( Event ):
    def process( self, queue ):
        stage = self._args[ 0 ]
        #catch up people
        #take down people

if __name__ == '__main__':
    simulator = ElevatorSimulator( 5, 1 )
    simulator.queue.push( InEvent( 0, simulator.stages[ 0 ], People( [ [ 1, 100 ], [3, 100], [] ] ) ) )
    simulator.run()
