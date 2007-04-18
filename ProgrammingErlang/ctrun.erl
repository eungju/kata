#!/usr/bin/env escript

main([Arg|_]) ->
	ct:start(list_to_atom(Arg)),
	receive
	after infinity ->
		false
	end.
