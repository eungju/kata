USING: arrays assocs formatting io kernel math math.order math.parser sequences
sorting tools.test unicode.case ;

IN: beautiful-strings

: normalize ( str -- str )
  >lower [ CHAR: a CHAR: z between? ] filter ;

"ABbCcc" normalize "abbccc" assert=
"Good luck in the Facebook Hacker Cup this year!" normalize "goodluckinthefacebookhackercupthisyear" assert=

: count-by ( seq -- assoc )
  H{ } clone swap [ over inc-at ] each ;

"abbccc" count-by H{ { CHAR: a 1 } { CHAR: b 2 } { CHAR: c 3 } } assert=

: make-best-index ( str -- assoc )
  count-by >alist [ [ second ] bi@ >=< ] sort keys [ 26 swap - 2array ] map-index ;

"abbccc" make-best-index { { CHAR: c 26 } { CHAR: b 25 } { CHAR: a 24 } } assert=

: best ( str -- beauty )
  normalize dup make-best-index swap [ over at ] map sum nip ;

"ABbCcc" best 152 assert=
"Good luck in the Facebook Hacker Cup this year!" best 754 assert=
"Ignore punctuation, please :)" best 491 assert=
"Sometimes test cases are hard to make up." best 729 assert=
"So I just go consult Professor Dalves" best 646 assert=

readln string>number iota [ 1 + readln best "Case #%d: %d\n" printf ] each