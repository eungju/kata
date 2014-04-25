import unittest

def encode(g):
	return (g[0], len(g))
	
def groups(l):
	group = []
	for e in l:
		if not group or group[-1] == e:
			group.append(e)
		else:
			old = group
			group = [e]
			yield encode(old)
	if group:
		yield encode(group)	

def sequence(seed):
	while True:
		seed = reduce(lambda x, y: x + y, groups(seed))
		yield seed
	
class SequenceTest(unittest.TestCase):
	def testGroups(self):
		self.assertEquals([(1, 1)], list(iter(groups([1]))))
		self.assertEquals([(1, 2)], list(iter(groups([1, 1]))))
		self.assertEquals([(1, 3)], list(iter(groups([1, 1, 1]))))
		self.assertEquals([(1, 1), (2, 1)], list(iter(groups([1, 2]))))
		self.assertEquals([(1, 2), (2, 1)], list(iter(groups([1, 1, 2]))))
		self.assertEquals([(1, 2), (2, 1), (1, 1)], list(iter(groups([1, 1, 2, 1]))))
	def testSequence(self):
		s = sequence([1])
		self.assertEquals((1, 1), s.next())
		self.assertEquals((1, 2), s.next())
		self.assertEquals((1, 1, 2, 1), s.next())

unittest.main(argv=('', '-v'))
