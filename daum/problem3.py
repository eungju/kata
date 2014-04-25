#!/usr/bin/env python
# encoding: utf-8

#estimation: 4h
#spend: 2h56m

import unittest

class CardNumber:
	all = []
	def __init__(self, id):
		self.id = id
		self.all.append(self)
	def next(self):
		return CardNumber(self.id + 1)
	def __repr__(self):
		return "23456789JQKA"[self.id - 2]
	def __cmp__(self, other):
		return self.id - other.id
			
NUMBER_2 = CardNumber(2)
NUMBER_3 = NUMBER_2.next()
NUMBER_4 = NUMBER_3.next()
NUMBER_5 = NUMBER_4.next()
NUMBER_6 = NUMBER_5.next()
NUMBER_7 = NUMBER_6.next()
NUMBER_8 = NUMBER_7.next()
NUMBER_9 = NUMBER_8.next()
NUMBER_J = NUMBER_9.next()
NUMBER_Q = NUMBER_J.next()
NUMBER_K = NUMBER_Q.next()
NUMBER_A = NUMBER_K.next()

#print CardNumber.all

class CardKind:
	all = []
	def __init__(self, id):
		self.id = id
		self.all.append(self)
	def next(self):
		return CardKind(self.id + 1)
	def __repr__(self):
		return "CHDS"[self.id - 1]
	def __cmp__(self, other):
		return self.id - other.id

KIND_C = CardKind(1)
KIND_H = KIND_C.next()
KIND_D = KIND_H.next()
KIND_S = KIND_D.next()

#print CardKind.all

class Card:
	def __init__(self, number, kind):
		self.number = number
		self.kind = kind
	def __cmp__(self, other):
		if self.number == other.number:
			return cmp(self.kind, other.kind)
		return cmp(self.number, other.number)
	def __repr__(self):
		return repr(self.number) + repr(self.kind)
		
for k in CardKind.all:
	for n in CardNumber.all:
		globals()[repr(k) + repr(n)] = Card(n, k)
		
assert NUMBER_2 == NUMBER_2
assert NUMBER_2 < NUMBER_3
assert NUMBER_A > NUMBER_K

assert KIND_S == KIND_S
assert KIND_S > KIND_D
assert KIND_D > KIND_H
assert KIND_H > KIND_C

assert D2.number == NUMBER_2
assert D2.kind == KIND_D

assert D3 > S2
assert D5 < S5
assert SA == max(S3, DA, SA, H9)

class Suit:
	def __init__(self, *cards):
		self.cards = list(cards)
	def __repr__(self):
		return "[" + " ".join(map(repr, self.cards)) + "]"
	def flush(self):
		sample = self.cards[0]
		for c in self.cards[1:]:
			if c.kind != sample.kind:
				return False
		return Flush(self.cards[:])
	def straight(self):
		sorted_cards = self.cards[:]
		sorted_cards.sort(lambda a, b: cmp(a.number, b.number))
		sample = sorted_cards[0]
		for c in sorted_cards[1:]:
			if sample.number.next() != c.number:
				return False
			sample = c
		return Straight(self.cards[:])
	def straightFlush(self):
		f = self.flush()
		s = self.straight()
		if f and s:
			return StraightFlush(f.cards)
		return False
	def fourOfAKind(self):
		for sample in self.cards:
			four = []
			for c in self.cards:
				if sample.number == c.number:
					four.append(c)
			if len(four) == 4:
				return FourCards(four)
		return False
	def fullHouse(self):
		for first_sample in self.cards:
			three = []
			for c in self.cards:
				if first_sample.number == c.number:
					three.append(c)
			if len(three) == 3:
				two = self.cards[:]
				for c in three: two.remove(c)
				if two[0].number == two[1].number:
					return FullHouse(three, two)
				return False
		return False
	def threeOfAKind(self):
		for sample in self.cards:
			three = []
			for c in self.cards:
				if sample.number == c.number:
					three.append(c)
			if len(three) == 3:
				return Triple(three)
		return False
	def twoPairs(self):
		for first_sample in self.cards:
			first_pair = []
			for c in self.cards:
				if first_sample.number == c.number:
					first_pair.append(c)
			if len(first_pair) == 2:
				remains = self.cards[:]
				for c in first_pair: remains.remove(c)
				for second_sample in remains:
					second_pair = []
					for c in remains:
						if second_sample.number == c.number:
							second_pair.append(c)
					if len(second_pair) == 2:
						return TwoPairs(first_pair, second_pair)
		return False
	def pair(self):
		for sample in self.cards:
			pair = []
			for c in self.cards:
				if sample.number == c.number:
					pair.append(c)
			if len(pair) == 2:
				return Pair(pair)
		return False
	def point(self):
		p = self.straightFlush()
		if p: return p
		p = self.fourOfAKind()
		if p: return p
		p = self.fullHouse()
		if p: return p
		p = self.flush()
		if p: return p
		p = self.straight()
		if p: return p
		p = self.threeOfAKind()
		if p: return p
		p = self.twoPairs()
		if p: return p
		p = self.pair()
		if p: return p
		return Plain(self.cards)
	def __cmp__(self, other):
		pointOfA = self.point()
		pointOfB = other.point()
		if pointOfA.priority == pointOfB.priority:
			return cmp(pointOfA, pointOfB)
		return pointOfA.priority - pointOfB.priority
		
class Plain:
	def __init__(self, cards):
		self.priority = 1
		self.cards = cards
	def __cmp__(self, other):
		return cmp(max(self.cards), max(other.cards))
		
class Pair:
	def __init__(self, cards):
		self.priority = 2
		self.cards = cards
	def __eq__(self, other):
		return self.cards == self.cards

class TwoPairs:
	def __init__(self, first, second):
		self.priority = 3
		self.first = first
		self.second = second
	def __eq__(self, other):
		return self.first == other.first and self.second == other.second
	
class Triple:
	def __init__(self, cards):
		self.priority = 4
		self.cards = cards
	def __eq__(self, other):
		return self.cards == self.cards

class Straight:
	def __init__(self, cards):
		self.priority = 5
		self.cards = cards
	def __eq__(self, other):
		return self.cards == self.cards

class Flush:
	def __init__(self, cards):
		self.priority = 6
		self.cards = cards
	def __eq__(self, other):
		return self.cards == self.cards
	
class FullHouse:
	def __init__(self, three, two):
		self.priority = 7
		self.three = three
		self.two = two
	def __eq__(self, other):
		return self.three == other.three and self.two == other.two

class FourCards:
	def __init__(self, cards):
		self.priority = 8
		self.cards = cards
	def __eq__(self, other):
		return self.cards == self.cards

class StraightFlush:
	def __init__(self, cards):
		self.priority = 9
		self.cards = cards
	def __eq__(self, other):
		return self.cards == self.cards
			
#Test Flush				
assert Flush([SA, S3, S2, S7, S9]) == Suit(SA, S3, S2, S7, S9).flush()
assert not Suit(SA, S3, D2, S7, S9).flush()

#Test Straight
assert Straight([CJ, DA, HQ, SK, C9]) == Suit(CJ, DA, HQ, SK, C9).straight()
assert not Suit(C9, DJ, HQ, SK, C3).straight()

#Test Straight flush
assert StraightFlush([CJ, CA, CQ, CK, C9]) == Suit(CJ, CA, CQ, CK, C9).straightFlush()
assert not Suit(S9, SJ, DQ, SK, SA).straightFlush()
assert not Suit(D9, DJ, DQ, DK, D3).straight()

#Test Four of a kind
assert FourCards([S7, D7, H7, C7]) == Suit(S7, D7, H7, C6, C7).fourOfAKind()
assert not Suit(S5, D7, H7, C7, C6).fourOfAKind()

#Test Full house
assert FullHouse([S3, H3, D3], [C8, H8]) == Suit(S3, H3, C8, D3, H8).fullHouse()
assert not Suit(S3, H3, C8, D3, H7).fullHouse()
assert not Suit(S3, H3, C8, D7, H7).fullHouse()

#Test Three of a kind
assert Triple([S7, D7, H7]) == Suit(S7, D7, H7, C6, C9).threeOfAKind()
assert not Suit(S5, D7, H7, C5, C6).threeOfAKind()

#Test Two pairs
assert TwoPairs([S3, H3], [C8, H8]) == Suit(S3, C8, D6, H8, H3).twoPairs()
assert not Suit(S3, C8, D6, H8, H4).twoPairs()

#Test pair
assert Pair([C8, H8]) == Suit(S2, C8, D6, H8, H3).pair()
assert not Suit(S2, C8, D6, H9, H3).pair()

#Test match
#assert Suit(H2, D3, S5, C9, DK) < Suit(C2, H3, S4, C8, HA)
#assert Suit(H2, S4, C4, D2, H4) > Suit(S2, S8, SA, SQ, S3)
#assert Suit(H2, D3, S5, C9, DK) > Suit(C2, H3, S4, C8, HK)
#assert Suit(H2, D3, S5, C9, DK) < Suit(C2, H3, C5, S9, HK)

def make_suit(text):
	suit = Suit()
	suit.cards = map(eval, map(lambda x: x[1] + x[0], text.split()))
	return suit
	
if __name__ == "__main__":
	import sys
	for line in sys.stdin.readlines():
		suitA, suitB = line.split("/")
		suitA = make_suit(suitA)
		suitB = make_suit(suitB)
		if suitA > suitB:
			print "A wins."
		else:
			print "B wins."
		