digit_carry := method(a, b, carryIn,
	((a + b + carryIn) / 10) floor
)

complexity := method(a, b, carryIn,
	if (a == 0 and b == 0,
		0,
		c := digit_carry(a % 10, b % 10, carryIn)
		c + complexity((a / 10) floor, (b / 10) floor, c)
	)
)

writeln(complexity(4, 5, 0))
writeln(complexity(5, 5, 0))
writeln(complexity(95, 5, 0))
writeln(complexity(56, 45, 0))

writeln(complexity(123, 456, 0))
writeln(complexity(555, 555, 0))
writeln(complexity(123, 594, 0))
