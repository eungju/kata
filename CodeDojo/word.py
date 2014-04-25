import unittest

def order(word):
    base = 'abcdefgh'
    for i in range(0, 8):
        d = ord(word[i]) - ord(base[i])
        if not d == 0:
            return (7 - i) + 1
    return 1

class PermTest(unittest.TestCase):
    def test1(self):
        self.assertEquals(1, order('abcdefgh'))
        self.assertEquals(2, order('abcdefhg'))
        self.assertEquals(40320, order('hgfedcba'))
        
#print order('abcdefhg')
#print order('hgfedcba')

if __name__ == '__main__':
    unittest.main(argv=('', '-v'))
