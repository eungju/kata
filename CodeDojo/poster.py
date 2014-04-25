import unittest

def overlap(l1, l2):
    start = min(l1[0], l2[0])
    end = max(l1[1], l2[1])
    x = (end - start) - abs(l2[0] - l1[0]) - abs(l2[1] - l1[1])
    if x < 0:
        return 0
    return x

def overlapArea(b1, b2):
    h1 = (b1[0], b1[2])
    h2 = (b2[0], b2[2])
    width = overlap(h1, h2)
    v1 = (b1[1], b1[3])
    v2 = (b2[1], b2[3])
    height = overlap(v1, v2)
    return width * height

def nooverlapArea(b1, b2):
    poster = (b1[2] - b1[0]) * (b1[3] - b1[1])
    return poster - overlapArea(b1, b2)
    
class PosterTest(unittest.TestCase):
    def testOverlap(self):
        self.assertEquals(2, overlap((1, 5), (3, 6)))
        self.assertEquals(2, overlap((3, 6), (1, 5)))
        self.assertEquals(0, overlap((1, 2), (3, 4)))
        self.assertEquals(0, overlap((3, 4), (1, 2)))
        self.assertEquals(1, overlap((2, 5), (4, 6)))
        self.assertEquals(1, overlap((3, 8), (7, 10)))
        self.assertEquals(1, overlap((1, 10), (2, 3)))
    def testOverlapArea(self):
        self.assertEquals(1, overlapArea((2, 3, 5, 8), (4, 7, 6, 10)))
    def testNooverlapArea(self):
        self.assertEquals(14, nooverlapArea((2, 3, 5, 8), (4, 7, 6, 10)))

def main():
    import sys
    n = int(sys.stdin.readline())
    for i in range(n):
        points = map(int, sys.stdin.readline().split())
        print nooverlapArea(points[:4], points[4:])
    
if __name__ == '__main__':
    unittest.main(argv=('', '-v'))
#    main()
