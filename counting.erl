-module(counting).
-compile([export_all]).

gustavo(D) when 1 =< D, D =< 3 ->
    D;
gustavo(D) when D =:= 4 ->
    1.

count(N) ->
    count(N, [], 0).
count(N, Trial, Count) ->
    %%io:format("~p~n", [Trial]),
    Sum = lists:sum(lists:map(fun gustavo/1, Trial)),
    if
	Sum > N ->
	    Count + 0;
	Sum =:= N ->
	    Count + 1;
	true ->
	    lists:foldl(fun(X, S) -> count(N, [X|Trial], S) end, Count, lists:seq(1, 4))
    end.

fac(N) ->
    fac(N, 1).
fac(0, Acc) ->
    Acc;
fac(N, Acc) ->
    fac(N - 1, N * Acc).

combination(A, B, C, D) ->
    fac(A + B + C + D) div (fac(A) * fac(B) * fac(C) * fac(D)).

count_fast(N) ->
    lists:sum([combination(A, B, C, D) ||
		  A <- lists:seq(0, N),
		  B <- lists:seq(0, (N - A) div 2),
		  C <- lists:seq(0, (N - A - 2 * B) div 3),
		  D <- lists:seq(0, N - A - 2 * B - 3 * C), (A + 2 * B + 3 * C + D) =:= N]).

count_fast_pmap(N) ->
    lists:sum(pmap(fun({A,B,C,D}) ->combination(A,B,C,D) end,
		   [{A, B, C, D} ||
		       A <- lists:seq(0, N),
		       B <- lists:seq(0, (N - A) div 2),
		       C <- lists:seq(0, (N - A - 2 * B) div 3),
		       D <- lists:seq(0, N - A - 2 * B - 3 * C), (A + 2 * B + 3 * C + D) =:= N],
		   [])).

pmap(Fun, List, Nodes) ->
    SpawnFun =
	case length(Nodes) of
	    0 -> fun spawn/1;
	    Length ->
		NextNode = fun() -> lists:nth(random:uniform(Length), Nodes) end,
		fun(F) -> spawn(NextNode(), F) end
	end,
    Parent = self(),
    Pids = [SpawnFun(fun() -> Parent ! {self(), (catch Fun(Elem))} end) || Elem <- List],
    [receive {Pid, Val} -> Val end || Pid <- Pids].

benchmark() ->
    {Micro, Result} = timer:tc(counting, count_fast, [500]),
    io:format("~ps~n", [Micro div 1000000]).
