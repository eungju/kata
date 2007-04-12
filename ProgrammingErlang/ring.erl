-module(ring).
-compile(export_all).

last([Last]) ->
    Last;
last([_|T]) ->
    last(T).

start(N) ->
    start(N, undefined, []).

start(0, Next, Spawned) ->
    last(Spawned) ! {route, Next},
    Spawned;
start(N, Next, Spawned) ->
    Pid = spawn(fun() -> loop(Next) end),
    %io:format("~w created~n", [Pid]),
    start(N - 1, Pid, [Pid|Spawned]).

loop(Next) ->
    receive
	{route, NewNext} ->
	    loop(NewNext);
	{stop} ->
	    true;
	{From, {in, Id}} ->
	    %io:format("~w: Pass ~w to ~w~n", [self(), Id, Next]),
	    Next ! {pass, Id, self(), From},
	    loop(Next);
	{pass, Id, Destination, Origin} when Destination =:= self() ->
	    %io:format("~w: Eat ~w~n", [self(), Id]),
	    Origin ! {out, Id},
	    loop(Next);
	{pass, Id, Destination, Origin} ->
	    %io:format("~w: Pass ~w to ~w~n", [self(), Id, Next]),
	    Next ! {pass, Id, Destination, Origin},
	    loop(Next)
    end.

send(Pid, Ids) ->
    lists:map(fun(Id) -> Pid ! {self(), {in, Id}} end, Ids).

wait([]) ->
    true;
wait([Id|T]) ->
    receive
	{out, Id} ->
	    wait(T)
    end.

stop(Pids) ->
    lists:map(fun(Pid) -> Pid ! {stop} end, Pids).

stopwatch_start() ->
    statistics(runtime),
    statistics(wall_clock).

stopwatch_stop() ->
    {_, Runtime} = statistics(runtime),
    {_, WallClock} = statistics(wall_clock),
    {Runtime, WallClock}.
    
bench(N, M) ->
    io:format("Start~n"),
    Pids = start(N),
    Ids = lists:seq(1, M),
    stopwatch_start(),
    send(hd(Pids), Ids),
    wait(Ids),
    {Runtime, WallClock} = stopwatch_stop(),
    stop(Pids),
    io:format("Stop ~w, ~w~n", [Runtime, WallClock]).
