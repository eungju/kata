-module(fac).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% Recursion
fac(0) -> 1;
fac(N) -> N * fac(N - 1).

%% Tail-recursion, Tail-call optimization, Accumulator Passing Style
fac_aps(0, Acc) -> Acc;
fac_aps(N, Acc) -> fac_aps(N - 1, Acc * N).
fac_aps(N) -> fac_aps(N, 1).

%% Continuation Passing Style
fac_cps(0, Cont) -> Cont(1);
fac_cps(N, Cont) -> fac_cps(N - 1, fun(X) -> Cont(N * X) end).
fac_cps(N) -> fac_cps(N, fun(X) -> X end).

fac_test_() ->
    [?_assertMatch(6, fac(3)),
     ?_assertMatch(6, fac_aps(3)),
     ?_assertMatch(6, fac_cps(3))].

time(Mod, Fun, Arg) ->
    {T, _} = timer:tc(Mod, Fun, Arg),
    T.

benchmark() ->
    Arg = [50000],
    %io:format("fac: ~pus~n", [time(?MODULE, fac, Arg)]),
    io:format("fac_aps: ~pus~n", [time(?MODULE, fac_aps, Arg)]),
    io:format("fac_cps: ~pus~n", [time(?MODULE, fac_cps, Arg)]).  
