import unittest

class Maze:
    def __init__(self, width):
        self.width = width
        self.cells = []
        for i in range(width):
            self.cells.append([None] * width)
    def engrave(self, aPosition, signature):
        x, y = aPosition
        self.cells[y][x] = signature
    def feel(self, aPosition):
        x, y = aPosition
        return self.cells[y][x]

class Direction:
    def __init__(self, vector):
        self.vector = vector
    def checkPassed(self, aBlind, aMaze):
        x = aBlind.position[0] + self.vector[0]
        y = aBlind.position[1] + self.vector[1]
        return aMaze.feel((x, y)) != None
        
class EastDirection(Direction):
    def __init__(self):
        Direction.__init__(self, (1, 0))
    def checkWall(self, aBlind, aMaze):
        return aBlind.position[0] == (aMaze.width - 1)
    def turnRight(self):
        return SOUTH

class SouthDirection(Direction):
    def __init__(self):
        Direction.__init__(self, (0, 1))
    def checkWall(self, aBlind, aMaze):
        return aBlind.position[1] == (aMaze.width - 1)
    def turnRight(self):
        return WEST

class WestDirection(Direction):
    def __init__(self):
        Direction.__init__(self, (-1, 0))
    def checkWall(self, aBlind, aMaze):
        return aBlind.position[0] == 0
    def turnRight(self):
        return NORTH

class NorthDirection(Direction):
    def __init__(self):
        Direction.__init__(self, (0, -1))
    def checkWall(self, aBlind, aMaze):
        return aBlind.position[1] == 0
    def turnRight(self):
        return EAST
        
EAST = EastDirection()
SOUTH = SouthDirection()
WEST = WestDirection()
NORTH = NorthDirection()

class Blind:
    def enter(self, aMaze):
        self.maze = aMaze
        self.position = (0, 0)
        self.direction = EAST
        self.count = 0
    def checkWall(self):
        return self.direction.checkWall(self, self.maze)
    def checkPassed(self):
        return self.direction.checkPassed(self, self.maze)
    def turnRight(self):
        self.direction = self.direction.turnRight()
    def forward(self):
        delta = self.direction.vector
        self.position = self.position[0] + delta[0], self.position[1] + delta[1]
    def feelTheWay(self):
        return self.checkWall() or self.checkPassed()
    def move(self):
        self.engrave()
        if self.feelTheWay():
            self.turnRight()
        self.forward()
    def engrave(self):
        self.count = self.count + 1
        self.maze.engrave(self.position, self.count)
    def canMove(self):
        return self.count == (self.maze.width ** 2) 
        
class MazeTest(unittest.TestCase):
    """test Directions instead of Blind"""
    def setUp(self):
        self.maze = Maze(3)
        self.blind = Blind()
    def testEnterMaze(self):
        self.blind.enter(self.maze)
        self.assertEquals((0, 0), self.blind.position)
    def testCheckEastWall(self):
        self.blind.enter(self.maze)
        self.blind.direction = EAST
        self.blind.position = (0, 1)
        self.assertFalse(self.blind.checkWall())
        self.blind.position = (2, 1)
        self.assertTrue(self.blind.checkWall())
    def testCheckSouthWall(self):
        self.blind.enter(self.maze)
        self.blind.direction = SOUTH
        self.blind.position = (1, 0)
        self.assertFalse(self.blind.checkWall())
        self.blind.position = (1, 2)
        self.assertTrue(self.blind.checkWall())
    def testCheckWestWall(self):
        self.blind.enter(self.maze)
        self.blind.direction = WEST
        self.blind.position = (1, 1)
        self.assertFalse(self.blind.checkWall())
        self.blind.position = (0, 1)
        self.assertTrue(self.blind.checkWall())
    def testCheckNorthWall(self):
        self.blind.enter(self.maze)
        self.blind.direction = NORTH
        self.blind.position = (1, 2)
        self.assertFalse(self.blind.checkWall())
        self.blind.position = (1, 0)
        self.assertTrue(self.blind.checkWall())
    def testEngrave(self):
        self.maze.engrave((0, 0), 1)
        self.assertEquals(1, self.maze.feel((0, 0)))
        self.assertEquals(None, self.maze.feel((1, 0)))
    def testCheckPassedEast(self):
        self.blind.enter(self.maze)
        self.blind.direction = EAST
        self.blind.position = (0, 0)
        self.assertFalse(self.blind.checkPassed())
        self.maze.engrave((1, 0), 1)
        self.assertTrue(self.blind.checkPassed())
    def testCheckPassedSouth(self):
        self.blind.enter(self.maze)
        self.blind.direction = SOUTH
        self.blind.position = (0, 0)
        self.assertFalse(self.blind.checkPassed())
        self.maze.engrave((0, 1), 1)
        self.assertTrue(self.blind.checkPassed())
    def testCheckPassedWest(self):
        self.blind.enter(self.maze)
        self.blind.direction = WEST
        self.blind.position = (1, 0)
        self.assertFalse(self.blind.checkPassed())
        self.maze.engrave((0, 0), 1)
        self.assertTrue(self.blind.checkPassed())
    def testCheckPassedNorth(self):
        self.blind.enter(self.maze)
        self.blind.direction = NORTH
        self.blind.position = (0, 1)
        self.assertFalse(self.blind.checkPassed())
        self.maze.engrave((0, 0), 1)
        self.assertTrue(self.blind.checkPassed())
    def testTurnRight(self):
        self.blind.direction = EAST
        self.blind.turnRight()
        self.assertEquals(SOUTH, self.blind.direction)
        self.blind.turnRight()
        self.assertEquals(WEST, self.blind.direction)
        self.blind.turnRight()
        self.assertEquals(NORTH, self.blind.direction)
        self.blind.turnRight()
        self.assertEquals(EAST, self.blind.direction)
    def testForwardEast(self):
        self.blind.position = (0, 0)
        self.blind.direction = EAST
        self.blind.forward()
        self.assertEquals((1, 0), self.blind.position)
    def testForwardSouth(self):
        self.blind.position = (0, 0)
        self.blind.direction = SOUTH
        self.blind.forward()
        self.assertEquals((0, 1), self.blind.position)
    def testForwardWest(self):
        self.blind.position = (1, 0)
        self.blind.direction = WEST
        self.blind.forward()
        self.assertEquals((0, 0), self.blind.position)
    def testForwardSouth(self):
        self.blind.position = (0, 1)
        self.blind.direction = NORTH
        self.blind.forward()
        self.assertEquals((0, 0), self.blind.position)

def main():
    maze = Maze(5)
    blind = Blind()
    blind.enter(maze)
    while not blind.canMove():
        blind.move()
    for y in range(maze.width):
        for x in range(maze.width):
            print "%3d" % maze.cells[y][x],
        print

def suite():
    classes = [
        MazeTest,
    ]
    return unittest.TestSuite([unittest.makeSuite(c) for c in classes])

if __name__ == "__main__":
    #main()
    unittest.TextTestRunner(verbosity=2).run(suite())
