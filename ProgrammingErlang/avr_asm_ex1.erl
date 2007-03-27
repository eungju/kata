-module(avr_asm_ex1).
-export([main/0]).

-define(temp1, r17).
-define(temp2, r18).

main() ->
    avr_asm:asm([
	 {ldi, ?temp1, 200},
	 {label, l1},
	 {ldi, ?temp2, 199},
	 {label, l2},
	 {dec, ?temp2},
	 {brne, l2},
	 {dec, ?temp1},
	 {brne, l1}
	]).
