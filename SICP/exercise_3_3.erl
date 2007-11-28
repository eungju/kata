-module(exercise_3_3).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

account(Balance) ->
    receive
	{withdraw, From, Amount} ->
	    NewBalance = Balance - Amount,
	    if
		NewBalance > 0 ->
		    From ! NewBalance,
		    account(NewBalance);
		true ->
		    From ! {error, underflow},
		    account(Balance)
	    end;
	{deposit, From, Amount} ->
	    NewBalance = Balance + Amount,
	    From ! NewBalance,
	    account(NewBalance);
	{_, From, _Amount} ->
	    From ! {error, unknown_request},
	    account(Balance)
    end.

make_account(Balance) ->
    Pid = spawn(?MODULE, account, [Balance]),
    fun(Action) ->
	    fun(Amount) ->
		    Pid ! {Action, self(), Amount},
		    receive
			{error, Why} ->
			    throw(Why);
			Result ->
			    Result
		    end
	    end
    end.

account_test_() ->
    A = make_account(100),
    [?_assertMatch(60, (A(withdraw))(40)),
     ?_assertMatch(80, (A(deposit))(20)),
     ?_assertMatch(underflow, catch((A(withdraw))(100))),
     ?_assertMatch(unknown_request, catch((A(robe))(20)))].

