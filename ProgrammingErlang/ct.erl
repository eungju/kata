%% TODOs
%% * Callback을 프로세스로 돌리기
%% * 돌고 있는 콜백이 있으면 죽이고 시작하기
%% * LAST_SNAPSHOT의 변경 시각을 현재 파일의 변경 시각과 비교하기
-module(ct).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(LATEST_SNAPSHOT, ".last").
-define(MTIME_PRECISION, 1000).
-define(TEST_FILE, "dut.txt").
-define(TEST_MODULE, dut).

is_modified(Filename, LastModified) ->
    case filelib:last_modified(Filename) of
	LastModified ->
	    false;
	_ -> true
    end.

sleep(Millisecond)->
    receive
    after Millisecond ->
	    void
    end.

touch(Filename) ->
    {ok, S} = file:open(Filename, write),
    io:format(S, "hello", []),
    file:close(S).

is_modified_test() ->
    touch(?TEST_FILE),
    Modified = filelib:last_modified("dut.txt"),
    ?assertMatch(false, is_modified("dut.txt", Modified)),
    sleep(?MTIME_PRECISION),
    touch(?TEST_FILE),
    ?assertMatch(true, is_modified("dut.txt", Modified)).

watch_loop(ModuleName, Callback) ->
    watch_loop(ModuleName, Callback, 0).

watch_loop(ModuleName, Callback, LastModified) ->    
    receive
	stop ->
	    true
    after ?MTIME_PRECISION ->
	    case is_modified(file_name(ModuleName), LastModified) of
		true ->
		    Callback(ModuleName);
		false ->
		    false
	    end,
	    ?MODULE:watch_loop(ModuleName, Callback,
		       filelib:last_modified(file_name(ModuleName)))
    end.

callback_test() ->
    Me = self(),
    Callback = fun(_) -> Me ! modified end,
    Pid = spawn(fun() -> watch_loop(?TEST_MODULE, Callback) end),
    touch(?TEST_FILE),
    Invoked = receive
		 modified ->
		     true
	     after 6000 ->
		     false
	     end,
    stop(Pid),
    ?assertMatch(true, Invoked).

diff(Old, New) ->
    os:cmd("diff -u " ++ Old ++ " " ++ New).

snapshot(FileName) ->
    file:copy(FileName, ?LATEST_SNAPSHOT).

run_test(ModuleName) ->
    case c:c(ModuleName) of
        {ok, ModuleName} ->
            apply(ModuleName, test, []);
	error ->
	    void
    end.

file_name(ModuleName) ->
    atom_to_list(ModuleName) ++ ".erl".

continuous_test(ModuleName) ->
    io:put_chars(diff(?LATEST_SNAPSHOT, file_name(ModuleName))),
    snapshot(file_name(ModuleName)),
    run_test(ModuleName).

start(ModuleName) ->
    spawn(ct, watch_loop, [ModuleName, fun continuous_test/1]).

stop(Pid) ->
    Pid ! stop.
