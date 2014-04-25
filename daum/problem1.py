#!/usr/bin/env python
# encoding: utf-8

#estimation: 1h
#spend: 1h10m

import unittest
import pprint

MATRIX_SIZE = 5

def floatings(fixed):
	return filter(lambda x: x not in fixed, range(1, MATRIX_SIZE + 1))
	
def is_fixed_element(e):
	return e != 0
	
def row_generator(seed):
	for perm in permutator(floatings(seed)):
		candidate = seed[:]
		pin_index = 0
		for hole_index in range(len(candidate)):
			if not is_fixed_element(candidate[hole_index]):
				candidate[hole_index] = perm[pin_index]
				pin_index = pin_index + 1
		yield candidate
	
def permutator(seed):
	if len(seed) <= 1:
			yield seed
	else:
		for perm in permutator(seed[1:]):
			for i in range(len(perm) + 1):
				yield perm[:i] + seed[0:1] + perm[i:]

def is_valid_candidate(state, row_candidate):
	for row_index in range(len(state)):
		for column_index in range(len(row_candidate)):
			if state[row_index][column_index] == row_candidate[column_index]:
				return False 
	return True
	
def solve(seed, current_state):
	row_index = len(current_state)
	if row_index == len(seed):
		return current_state
	else:
		for row in row_generator(seed[row_index]):
			if not is_valid_candidate(current_state, row):
				continue
			next_state = current_state[:]
			next_state.append(row)
			solution = solve(seed, next_state)
			if solution : return solution
		return None
		
class MatrixTest(unittest.TestCase):
	def testRowGenerator(self):
		g = row_generator([1, 0, 0, 2, 0])
		self.assertEquals([1, 3, 4, 2, 5], g.next())
		self.assertEquals([1, 4, 3, 2, 5], g.next())
		self.assertEquals([1, 4, 5, 2, 3], g.next())
	def testPermutator(self):
		g = permutator([3, 4, 5])
		self.assertEquals([3, 4, 5], g.next())
		self.assertEquals([4, 3, 5], g.next())
		self.assertEquals([4, 5, 3], g.next())

def solve_the_problem():
	THE_PROBLEM_MATRIX = [[1,0,0,2,0], [0,0,0,0,3], [0,0,5,0,0], [0,3,0,5,0], [4,0,0,0,5]] 
	pprint.pprint(solve(THE_PROBLEM_MATRIX, []))
	
if __name__ == '__main__':
	#unittest.main()
	solve_the_problem()