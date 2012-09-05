-module(p2_1).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% 주어진 자연수 n의 팩토리얼을 구해서 마지막에 붙은 연속된 0의 개수와 연속된 0 바로 앞에 나오는 숫자를 구하는 문제이다. 우선 문제 설명대로 팩토리얼을 구하고 마지막 연속된 0의 갯수를 세고, 마지막 연속된 0을 제외한 마지막 자리수를 구한다.

% 팩토리얼을 구한다.

factorial(1, Acc) ->
    Acc;
factorial(N, Acc) ->
    factorial(N - 1, N * Acc).
factorial(N) ->
    factorial(N, 1).

factorial_test_() ->
    [?_assertMatch(1, factorial(1)),
     ?_assertMatch(2, factorial(2)),
     ?_assertMatch(120, factorial(5))
    ].

% 마지막에 연속된 0의 갯수를 구한다.

trailing_zeros(N, Zeros) when N >= 10, N rem 10 =:= 0 ->
    trailing_zeros(N div 10, Zeros + 1);
trailing_zeros(_, Zeros) ->
    Zeros.
trailing_zeros(N) ->
    trailing_zeros(N, 0).

trailing_zeros_test_() ->
    [?_assertMatch(0, trailing_zeros(1)),
     ?_assertMatch(1, trailing_zeros(10)),
     ?_assertMatch(0, trailing_zeros(11))
    ].

% 마지막에 연속된 0을 제외한 마지막 자리 수를 구한다.

significand(N) when N >= 10, N rem 10 =:= 0 ->
    significand(N div 10);
significand(N) ->
    N.

significand_test_() ->
    [?_assertMatch(1, significand(1)),
     ?_assertMatch(1, significand(10)),
     ?_assertMatch(12, significand(12))
    ].

last_digit(N) ->
    N rem 10.

last_digit_test_() ->
   [?_assertMatch(1, last_digit(1)),
    ?_assertMatch(2, last_digit(12)),
    ?_assertMatch(3, last_digit(123))
   ].
    

% trailing_zeros와 siginificand는 문제의 크기를 줄이는 방법이 동일하므로 하나로 합칠 수 있다.

significand_exponent(N, Zeros) when N >= 10, N rem 10 =:= 0 ->
    significand_exponent(N div 10, Zeros + 1);
significand_exponent(N, Zeros) ->
    {N, Zeros}.
significand_exponent(N) ->
    significand_exponent(N, 0).

significand_exponent_test_() ->
    [?_assertMatch({1, 0}, significand_exponent(1)),
     ?_assertMatch({1, 1}, significand_exponent(10)),
     ?_assertMatch({11, 0}, significand_exponent(11)),
     ?_assertMatch({12, 1}, significand_exponent(120)),
     ?_assertMatch({12, 2}, significand_exponent(1200))
    ].


% 이를 모두 조합하여 문제를 푼다.

bruteforce(N) ->
    F = factorial(N),
    {S, E} = significand_exponent(F),
    {E, last_digit(S)}.

bruteforce_test_() ->
    [?_assertMatch({3, 8}, bruteforce(15)),
     ?_assertMatch({4, 4}, bruteforce(20)),
     ?_assertMatch({7, 8}, bruteforce(30)),
     ?_assertMatch({9, 2}, bruteforce(40)),
     ?_assertMatch({12, 2}, bruteforce(50)),
     ?_assertMatch({24, 4}, bruteforce(100)),
     ?_assertMatch({501, 8}, bruteforce(2012)),
     ?_assertMatch({2499, 8}, bruteforce(10000))
    ].

% Erlang은 자릿수 제한 없는 연산을 제공하기 때문에 이렇게 풀어도 문제에서 제시한 1 <= N <= 10000 입력에 대해 올바른 답을 빨리 구할 수 있다. 내 노트북에서 N = 10000일 때도 1초 정도 걸린다. 하지만 문제 조건에 자릿수 제한 없는 연산을 써서 답을 구하는 것은 의도가 아니라고 했으므로 다른 방법을 찾자.

% 자릿수 무제한 연산을 사용하지 않으려면 숫자가 커지는 것을 막아야 한다. 팩토리얼을 구할 때 마지막 0이 붙어서 자릿수가 늘어나지 않도록 해보자.

optimized(1, Acc, Zeros) ->
    {Zeros, last_digit(Acc)};
optimized(N, Acc, Zeros) ->
    X = N * Acc,
    {S, E} = significand_exponent(X),
    optimized(N - 1, S, Zeros + E).
optimized(N) ->
    optimized(N, 1, 0).

optimized_test_() ->
    [?_assertMatch({3, 8}, optimized(15)),
     ?_assertMatch({4, 4}, optimized(20)),
     ?_assertMatch({7, 8}, optimized(30)),
     ?_assertMatch({9, 2}, optimized(40)),
     ?_assertMatch({12, 2}, optimized(50)),
     ?_assertMatch({24, 4}, optimized(100)),
     ?_assertMatch({501, 8}, optimized(2012)),
     ?_assertMatch({2499, 8}, optimized(10000))
    ].
    
% 팩토리얼을 구하는 과정에서 마지막 0이 제거되기 때문에 숫자가 전보다는 훨씬 느리게 커진다. 하지만 입력이 20만 되어도 숫자는 4 바이트 정수형으로 표현하지 못할 만큼 크다. 답을 구할 때 필요한 자리는 마지막 한 자리이다. 곱하기 연산에서 낮은 자리수를 곱한 결과가 높은 자리수에 올림이 되지만 높은 자리수를 곱한 결과가 낮은 자리수로 내려오지는 않는다. 그러므로 0의 갯수나 마지막 자리수에 영향을 주는 마지막 숫자들을 제외한 높은 자리의 수는 버려도 된다.

optimized_more(1, Acc, Zeros) ->
    {Zeros, Acc};
optimized_more(N, Acc, Zeros) ->
    X = N * Acc,
    {S, E} = significand_exponent(X),
    Bound = round(math:pow(10, round(math:log10(N)) + 1)),
    optimized_more(N - 1, S rem Bound, Zeros + E).
optimized_more(N) ->
    optimized_more(N, 1, 0).

optimized_more_test_() ->
    [?_assertMatch({3, 8}, optimized_more(15)),
     ?_assertMatch({4, 4}, optimized_more(20)),
     ?_assertMatch({7, 8}, optimized_more(30)),
     ?_assertMatch({9, 2}, optimized_more(40)),
     ?_assertMatch({12, 2}, optimized_more(50)),
     ?_assertMatch({24, 4}, optimized_more(100)),
     ?_assertMatch({501, 8}, optimized_more(2012)),
     ?_assertMatch({2499, 8}, optimized_more(10000))
    ].

% 팩토리얼을 구하지 않으므로 입력이 1000000일 때도 바로 결과가 나온다.

