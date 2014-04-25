#!/usr/bin/env python

import unittest

def CountCarry(a, b, carryIn=0):
    radix = 10
    if a == 0 and b == 0:
        return 0
    elif a < radix and b < radix:
        return (a + b + carryIn) / radix
    carryOut = CountCarry(a % radix, b % radix, carryIn)
    return CountCarry(a / radix, b / radix, carryOut) + carryOut

class PrimaryArithmeticTest(unittest.TestCase):
    def testSingleDigit(self):
        self.assertEquals(0, CountCarry(3, 4))
        self.assertEquals(1, CountCarry(5, 6))
    def testSingleDigitWithCarryIn(self):
        self.assertEquals(0, CountCarry(3, 4, 0))
        self.assertEquals(1, CountCarry(3, 6, 1))
    def testMultipleDigits(self):
        self.assertEquals(0, CountCarry(3, 4))
        self.assertEquals(1, CountCarry(5, 6))
        self.assertEquals(0, CountCarry(13, 14))
        self.assertEquals(1, CountCarry(53, 64))
        self.assertEquals(2, CountCarry(56, 45))

unittest.main()
"""import sys

while True:
    a, b = [int(x) for x in sys.stdin.readline().split()]
    if a == 0 and b == 0:
        break
    carryCount = CountCarry(a, b)
    if carryCount == 0:
        print "No carry operation."
    elif carryCount == 1:
        print "%d carry operation." % carryCount
    else:
        print "%d carry operations." % carryCount
 """