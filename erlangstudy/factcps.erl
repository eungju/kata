-module(factcps).
-compile(export_all).

%Continuation Passing Style   CPS  CSP Communicating Sequential Processes 

fact(0,Co)-> Co(1);
fact(N,Co)->
	fact(N-1,fun(X) -> Co(N*X) end).

fact(N) -> fact(N, fun(X)->X end).



