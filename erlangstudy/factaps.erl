-module(factaps).
-compile(export_all).

fact(0,Acc)->Acc;
fact(N,Acc)->fact(N-1,N*Acc).

fact(N)->fact(N,1).
