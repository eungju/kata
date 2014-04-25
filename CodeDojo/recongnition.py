import unittest

def searchMaxSum(vector):
    if vector[0] < 0:
        return []
    else:
        return vector

class MaxSumTest(unittest.TestCase):
    def testAllPositive(self):
        self.assertEquals([3], searchMaxSum([3]))
    def testAllNegative(self):
        self.assertEquals([], searchMaxSum([-1]))

if __name__ == '__main__':
    unittest.main(argv=('', '-v'))
