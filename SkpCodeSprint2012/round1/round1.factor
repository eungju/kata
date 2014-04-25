#! /usr/bin/env factor
USING: alien.c-types alien.data command-line io io.encodings.utf8
io.files math.parser sequences sorting.insertion.private splitting ;
IN: round1

: string>edge ( line -- edge ) "," split [ string>number ] map ;

: graph-file ( -- path ) (command-line) second ;
: source-nodes ( -- seq ) (command-line) rest rest [ string>number ] map ;

"graph" <sqlite-db> [
    graph-file utf8 [
        [ string>edge [ but-last uint >c-array ] [ third ] gdbm:insert ] each-line
    ] with-file-reader
] with-db

