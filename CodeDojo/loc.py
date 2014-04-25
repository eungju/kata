import unittest
from StringIO import StringIO


class Code:
    def __init__(self, source):
        self.source = source
    def removeComments(self):
        pos = 0
        while pos < len(self.source):
            pos += 1
    
code = Code("code//comment")
print code.removeComments()
print "OK"
