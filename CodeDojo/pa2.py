#!/usr/bin/env python

import unittest

class Digit:
    def __init__(self, value):
        self.value = value
    def add(self, digit, carryIn=0):
        sum = self.value + digit.value + carryIn
        return [sum / 10, Digit(sum % 10)]
    def __eq__(self, other):
        return self.value == other.value
    def __repr__(self):
        return "Digit %d" % self.value

class Number:
    def __init__(self, value):
        self.digits = [Digit(value)]
    def add(self, number):
        carryOut, sum = self.digits[0].add(number.digits[0])
        return [Digit(carryOut), sum]
    def __eq__(self, other):
        return self.digits == other.digits
    
class PrimaryArithmeticTest(unittest.TestCase):
    def testDigitAddition(self):
        three = Digit(3)
        four = Digit(4)
        self.assertEquals([0, Digit(7)], three.add(four))
        self.assertEquals([1, Digit(1)], Digit(5).add(Digit(6)))
    def testDigitAdditionWithCarry(self):
        self.assertEquals([0, Digit(8)], Digit(3).add(Digit(4), 1))
        self.assertEquals([1, Digit(2)], Digit(5).add(Digit(6), 1))
    def testDecimalAddition(self):
        self.assertEquals([Digit(0), Digit(7)], Number(3).add(Number(4)))
        self.assertEquals([Digit(1), Digit(1)], Number(5).add(Number(6)))

unittest.main()