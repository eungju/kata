-module(fib).
-compile(export_all).

fib(N) when N =:= 0 ->
    0;
fib(N) when N =:= 1->
    1;
fib(N) when N > 1 ->
    fib(N - 1) + fib(N - 2).
