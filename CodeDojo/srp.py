import unittest

WIN = 1
LOSE = -1
TIE = 0

class Rock:
    def match(self, other):
        return other.matchWithRock()
    def matchWithPaper(self):
        return WIN
    def matchWithRock(self):
        return TIE
    def matchWithScissors(self):
        return LOSE
ROCK = Rock()
    
class Scissors:
    def match(self, other):
        return other.matchWithScissors()
    def matchWithRock(self):
        return WIN
    def matchWithScissors(self):
        return TIE
    def matchWithPaper(self):
        return LOSE
SCISSORS = Scissors()

class Paper:
    def match(self, other):
        return other.matchWithPaper()
    def matchWithScissors(self):
        return WIN
    def matchWithPaper(self):
        return TIE
    def matchWithRock(self):
        return LOSE
PAPER = Paper()

class GameTest(unittest.TestCase):
    def testTie(self):
        self.assertEquals(TIE, SCISSORS.match(SCISSORS))
        self.assertEquals(TIE, ROCK.match(ROCK))
        self.assertEquals(TIE, PAPER.match(PAPER))
    def testWin(self):
        self.assertEquals(WIN, SCISSORS.match(PAPER))
        self.assertEquals(WIN, ROCK.match(SCISSORS))
        self.assertEquals(WIN, PAPER.match(ROCK))
    def testLose(self):
        self.assertEquals(LOSE, SCISSORS.match(ROCK))
        self.assertEquals(LOSE, ROCK.match(PAPER))
        self.assertEquals(LOSE, PAPER.match(SCISSORS))
        
if __name__ == "__main__":
    unittest.main(argv=('', '-v'))
