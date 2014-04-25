assert := method(
	actualExpression := thisMessage argAt(0)
	expectedExpression := thisMessage argAt(1)
	actual := self clone doMessage(actualExpression)
	expected := sender doMessage(expectedExpression)
	if(expected != actual,
		writeln(self type .. ": ", actualExpression code, " ==> ", actual, " not ", expected)
		Error raise("Assert Failed", self type .. ": " .. actualExpression code)
	)
)