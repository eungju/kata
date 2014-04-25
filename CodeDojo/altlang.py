class Groups:
    def __init__(self):
        self.elements = []
    def add(self, group):
        self.elements.append(group)
    def merge(self):
        return reduce(lambda a, b: a + b, self.elements)
        
class Sequence:
    def __init__(self, elements):
        self.elements = elements
    def groups(self):
        groups = Groups()
        group = [self.elements[0]]
        for e in self.elements[1:]:
            if group[-1] == e:
                group.append(e)
            else:
                groups.add([group[0], len(group)])
                group = [e]
        groups.add([group[0], len(group)])
        return groups
        
assert [1, 1] == Sequence([1]).groups().merge()
assert [1, 2] == Sequence([1, 1]).groups().merge()

print "OK"


