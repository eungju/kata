-module(chat).
-compile(export_all).

start() ->
    Pid = spawn(fun() ->
			receive_loop() end),
    register(chat, Pid).

receive_loop() ->
    receive
	Any ->
	    io:format("XXX: ~p~n", [Any]),
	    ?MODULE:receive_loop()
    end.
