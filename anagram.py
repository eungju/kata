import unittest

class Anagram:
    def __init__(self):
        self._anagrams = {}
    def add(self, word):
        s = self._signature(word)
        if self._anagrams.has_key(s):
            self._anagrams[s].append(word)
        else:
            self._anagrams[s] = [word]
    def anagrams(self):
        return self._anagrams.values()
    def _signature(self, word):
        s = list(word)
        s.sort()
        return "".join(s)

class AnagramTestCase(unittest.TestCase):
    def test1(self):
        anagram = Anagram()
        anagram.add('abc')
        self.assert_(['abc'] in anagram.anagrams())
    def test2(self):
        anagram = Anagram()
        anagram.add('abc')
        anagram.add('bac')
        self.assert_(['abc', 'bac'] in anagram.anagrams())
    def test3(self):
        anagram = Anagram()
        anagram.add('efg')
        anagram.add('abc')
        anagram.add('bac')
        self.assert_(['abc', 'bac'] in anagram.anagrams())
        self.assert_(['efg'] in anagram.anagrams())

def main():
    import sys
    import time
    t1 = time.clock()
    anagram = Anagram()
    for word in sys.stdin:
        anagram.add(word.strip())
    for words in anagram.anagrams():
        print ' '.join(words)
    t2 = time.clock()
    print t2 - t1
    
if __name__ == "__main__":
    unittest.main(argv=('', '-v'))
