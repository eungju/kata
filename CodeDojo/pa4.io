DigitAdder := Object clone
DigitAdder carryOut := 0
DigitAdder new := method(a, b, prevAdder,
	self := DigitAdder clone
	carryOut = (((a + b + (prevAdder carryOut)) / 10) floor)
	return self
	)

complexity := method(a, b,
	aSeq := a asString
	bSeq := b asString
	width := aSeq size max(bSeq size)

	adders := List clone
	nullAdder := DigitAdder clone
	adders append(nullAdder)
	for (i, 1, width,
		a := if ((aSeq size - i) < 0, 0, aSeq at(aSeq size - i) - 48)
		b := if ((bSeq size - i) < 0, 0, bSeq at(bSeq size - i) - 48)
		adder := DigitAdder new(a, b, adders at(i - 1))
		adders append(adder)
		)

	carries := 0
	for (i, 1, width,
		carries = carries + adders at(i) carryOut
		)
	return carries
	)

#adder := DigitAdder new(5, 4, 1)
#adder carryOut print

writeln(complexity(55, 45))
writeln(complexity(1, 999999))
writeln(complexity(123, 456))
writeln(complexity(555, 555))
writeln(complexity(123, 594))