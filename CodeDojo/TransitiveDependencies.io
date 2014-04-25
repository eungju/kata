Importer turnOff 
doFile("Assert.io")

Dependencies := Object clone do(
    dependencyMap := Map()
    clear := method(dependencyMap empty)
    add := method(
        module := thisMessage argAt(0) name
        deps := list()
        i := 1
        while (thisMessage argAt(i),
            deps append(thisMessage argAt(i) name)
            i = i + 1
        )
        dependencyMap atPut(module, deps)
    )
    dependencies := method(
        module := thisMessage argAt(0) name
        indirects := list()
        seed := list(module)
        while (Nil isNil,
            nextSeed := list()
            seed foreach (s,
                directs := dependencyMap at(s) or list()
                directs foreach (v, (indirects contains(v) or (module == v)) ifNil (
                        nextSeed append(v)
                    )
                )
            )
            if (nextSeed size == 0, break)
            indirects appendSeq(nextSeed)
            seed = nextSeed
        )
        indirects
    )
)


d := Dependencies clone

//direct
d add(A, B)
assert(d dependencies(A), list("B"))
d add(A, B, C)
assert(d dependencies(A), list("B", "C"))

//transitive
d clear
d add(A, B)
d add(B, C)
assert(d dependencies(A), list("B", "C"))

//circular
d clear
d add(A, B)
d add(B, C)
d add(C, A)
assert(d dependencies(A), list("B", "C"))
assert(d dependencies(B), list("C", "A"))
assert(d dependencies(C), list("A", "B"))

//basic
d clear
d add (A, B, C)
d add (B, C, E)
d add (C, G)
d add (D, A, F)
d add (E, F)
d add (F, H)

assert ( list("B", "C", "E", "F", "G", "H" },   d dependencies(A) sort)
assert ( list("C", "E", "F", "G", "H" },     d dependencies(B) sort)
assert ( list("G"},             d dependencies(C))
assert ( list("A", "B", "C", "E", "F", "G", "H" }, d dependencies(D) sort)
assert ( list("F", "H" },           d dependencies(E) sort)
assert ( list("H" },             d dependencies(F))
    
writeln("OK.")
