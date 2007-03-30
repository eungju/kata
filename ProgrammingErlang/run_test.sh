#!/bin/sh

MODULE=$1

erlc $MODULE.erl
erl -eval "$MODULE:test()." -noshell -s init stop

