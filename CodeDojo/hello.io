write("Hello, World!\n")

assert := method(
    actualExpression := thisMessage argAt(0)
    expectedExpression := thisMessage argAt(1)
    actual := self clone doMessage(actualExpression)
    expected := sender doMessage(expectedExpression)
    if(actual != expected,
        writeln(self type .. ": ", actualExpression code, " ==> ", actual, " not ", expected)
        Error raise("Assert Failed", self type .. ": " .. actualExpression code)
    )
)

assert(1 + 1, 2)
assert(1 + 2, 2)