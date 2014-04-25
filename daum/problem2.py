#!/usr/bin/env python
# encoding: utf-8

#estimation: 3h
#spend: 3h48m

import copy
import unittest
import pprint

C = 1
Q = 8
				
def count_queens(state):
	queens = 0
	size = len(state)
	for y in xrange(size):
		for x in xrange(size):
			if state[y][x] == Q:
				queens = queens + 1
	return queens		

class Board:
	def __init__(self, size):
		self.size = size
		self.cells = []
		for r in xrange(self.size):
			self.cells.append([0] * self.size)
	def setCreep(self, row, column):
		self.cells[row][column] = C
	def clone(self):
		return copy.deepcopy(self)
	def is_valid_candidate(self, state, candidate):
		row = len(state)
		for column in xrange(self.size):
			if candidate[column] != Q:
				continue
			#check vertical
			for i in xrange(1, row + 1):
				if state[row - i][column] == C:
					break
				if state[row - i][column] == Q:
					return False
			#check up-left diagonal
			for i in xrange(1, row + 1):
				if (column - i) >= 0 and state[row - i][column - i] == C:
					break
				if (column - i) >= 0 and state[row - i][column - i] == Q:
					return False
			#check up-right diagonal
			for i in xrange(1, row + 1):
				if (column + i) < self.size and state[row - i][column + i] == C:
					break
				if (column + i) < self.size and state[row - i][column + i] == Q:
					return False
		return True
	def solve(self, state):
		row = len(state)
		if row == self.size:
			if count_queens(state) >= self.size:
				yield state
#			else:
#				yield None
		else:
			for candidate in row_generator(self.cells[row]):
				if not self.is_valid_candidate(state, candidate):
					continue
				nextState = state[:]
				nextState.append(candidate)
				for solution in self.solve(nextState):
					#if solution: yield solution
					yield solution
			#yield None
		
def row_generator(row):
	if C in row:
		e = row.index(C)
		for tail in row_generator(row[e + 1:]):
			yield row[:e] + [C] + tail
			for i in xrange(e):
				head = row[:e]
				head[i] = Q
				yield head + [C] + tail
	else:
		yield row
		for i in xrange(len(row)):
			candidate = row[:]
			candidate[i] = Q
			yield candidate

	
class QueenTest(unittest.TestCase):
	def testCreep(self):
		board = Board(2)
		board.setCreep(0, 0)
		self.assertEqual(C, board.cells[0][0])
		self.assertEqual(0, board.cells[0][1])
	def testNoCreep(self):
		self.assertEquals([[0,0], [Q,0], [0,Q]], list(row_generator([0,0])))
		self.assertEquals([[0,0,0], [Q,0,0], [0,Q,0], [0,0,Q]], list(row_generator([0,0,0])))
	def testOneCreep(self):
		self.assertEquals([[C,0],[C,Q]], list(row_generator([C,0])))
		self.assertEquals([[0,C],[Q,C]], list(row_generator([0,C])))
		self.assertEquals([[0,C,0], [Q,C,0], [0,C,Q], [Q,C,Q]], list(row_generator([0,C,0])))
		
def solve_the_problem():
	board = Board(8)
	board.setCreep(0, 0)
	board.setCreep(2, 2)
	board.setCreep(5, 5)
	i = 1
	for solution in board.solve([]):
		print "Solution %d, %d queens" % (i, count_queens(solution))
		pprint.pprint(solution)
		i = i + 1

if __name__ == '__main__':
	#unittest.main()
	solve_the_problem()
