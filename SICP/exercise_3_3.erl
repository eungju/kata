-module(exercise_3_3).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

account(Balance) ->
    receive
	{withdraw, From, Amount} when Balance < Amount ->
	    From ! {error, underflow},
	    account(Balance);
	{withdraw, From, Amount} ->
	    account(From ! Balance - Amount);
	{deposit, From, Amount} ->
	    account(From ! Balance + Amount);
	{_, From, _Amount} ->
	    From ! {error, unknown_request},
	    account(Balance)
    end.

make_account(Balance, Password) ->
    Pid = spawn(?MODULE, account, [Balance]),
    fun(Action, P) when Password =:= P ->
	    fun(Amount) ->
		    Pid ! {Action, self(), Amount},
		    receive
			{error, Why} ->
			    throw(Why);
			Result ->
			    Result
		    end
	    end;
       (_Action, _P) ->
	    fun(_Amount) ->
		    throw(incorrect_password)
	    end
    end.

account_test_() ->
    A = make_account(100, secret_password),
    [?_assertMatch(60, (A(withdraw, secret_password))(40)),
     ?_assertMatch(80, (A(deposit, secret_password))(20)),
     ?_assertMatch(underflow, catch((A(withdraw, secret_password))(100))),
     ?_assertMatch(unknown_request, catch((A(robe, secret_password))(20))),
     ?_assertMatch(incorrect_password, catch((A(withdraw, wrong_password))(20)))].
