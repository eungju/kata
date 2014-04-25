import unittest

def merge(a, b):
    indexOfA = 0
    indexOfB = 0
    merged = []
    while len(merged) < (len(a) + len(b)):
        if indexOfA == len(a):
            merged.append(b[indexOfB])
            indexOfB = indexOfB + 1
        elif indexOfB == len(b):
            merged.append(a[indexOfA])
            indexOfA = indexOfA + 1
        elif a[indexOfA] < b[indexOfB]:
            merged.append(a[indexOfA])
            indexOfA = indexOfA + 1
        else:
            merged.append(b[indexOfB])
            indexOfB = indexOfB + 1
    return merged

def merge_sort(a):
    if len(a) <= 1:
        return a
    else:
        m = len(a) / 2
        return merge(merge_sort(a[:m]), merge_sort(a[m:]))
        
class SortTest(unittest.TestCase):
    def testMerge(self):
        self.assertEquals([1, 2], merge([1], [2]))
    def testMerge2(self):
        self.assertEquals([1, 2, 3], merge([1, 2], [3]))
    def testMerge3(self):
        self.assertEquals([1, 2, 3], merge([2, 3], [1]))
    def testSort1(self):
        self.assertEquals([1], merge_sort([1]))
    def testSort2(self):
        self.assertEquals([1, 2, 3], merge_sort([2, 3, 1]))
    def testSort3(self):
        self.assertEquals([1, 2, 3, 4], merge_sort([4, 3, 2, 1]))
        
if __name__ == "__main__":
    unittest.main()
