-module(avr_asm).
-export([asm/1]).
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_unused_vars).
    
instruction(Name) ->
    case Name of
	adc ->
	    {1, fun(Labels, A, {Rd5, Rr5}) -> {register_addr(Rd5), register_addr(Rr5)} end};
	add ->
	    {1, fun(Labels, A, {Rd5, Rr5}) -> {register_addr(Rd5), register_addr(Rr5)} end};
	adiw ->
	    {1, fun(Labels, A, {Rd2, K6}) -> {(register_addr(Rd2) - 24) div 2, K6} end};
	and_ ->
	    {1, fun(Labels, A, {Rd5, Rr5}) -> {register_addr(Rd5), register_addr(Rr5)} end};
	nop ->
	    {1, fun(Labels, A, {}) -> {} end};
	sec ->
	    {1, fun(Labels, A, {}) -> {} end};
	bset ->
	    {1, fun(Labels, A, {S3}) -> {S3} end};
	bclr ->
	    {1, fun(Labels, A, {S3}) -> {S3} end};
	ser ->
	    {1, fun(Labels, A, {Rd4}) -> {register_addr(Rd4)} end};
	com ->
	    {1, fun(Labels, A, {Rd5}) -> {register_addr(Rd5)} end};
	tst ->
	    {1, fun(Labels, A, {Rd10}) -> {register_addr(Rd10)} end};
	brbs ->
	    {1, fun(Labels, A, {S3, K}) -> {S3, pc_relative_addr(labels_fetch(K, Labels), A)} end};
	brbc ->
	    {1, fun(Labels, A, {S3, K}) -> {S3, pc_relative_addr(labels_fetch(K, Labels), A)} end};
	ldi ->
	    {1, fun(Labels, A, {Rd, K}) -> {register_addr(Rd), K} end};
	dec ->
	    {1, fun(Labels, A, {Rd}) -> {register_addr(Rd)} end};
	brne ->
	    {1, fun(Labels, A, {K}) -> {pc_relative_addr(labels_fetch(K, Labels), A)} end}
    end.

%	MNEMONIC_NOP = 0,  //          0000 0000 0000 0000
%	MNEMONIC_SEC,      //          1001 0100 0000 1000
%	MNEMONIC_CLC,      //          1001 0100 1000 1000
%	MNEMONIC_SEN,      //          1001 0100 0010 1000
%	MNEMONIC_CLN,      //          1001 0100 1010 1000
%	MNEMONIC_SEZ,      //          1001 0100 0001 1000
%	MNEMONIC_CLZ,      //          1001 0100 1001 1000
%	MNEMONIC_SEI,      //          1001 0100 0111 1000
%	MNEMONIC_CLI,      //          1001 0100 1111 1000
%	MNEMONIC_SES,      //          1001 0100 0100 1000
%	MNEMONIC_CLS,      //          1001 0100 1100 1000
%	MNEMONIC_SEV,      //          1001 0100 0011 1000
%	MNEMONIC_CLV,      //          1001 0100 1011 1000
%	MNEMONIC_SET,      //          1001 0100 0110 1000
%	MNEMONIC_CLT,      //          1001 0100 1110 1000
%	MNEMONIC_SEH,      //          1001 0100 0101 1000
%	MNEMONIC_CLH,      //          1001 0100 1101 1000
%	MNEMONIC_SLEEP,    //          1001 0101 1000 1000
%	MNEMONIC_WDR,      //          1001 0101 1010 1000
%	MNEMONIC_IJMP,     //          1001 0100 0000 1001
%	MNEMONIC_EIJMP,    //          1001 0100 0001 1001
%	MNEMONIC_ICALL,    //          1001 0101 0000 1001
%	MNEMONIC_EICALL,   //          1001 0101 0001 1001
%	MNEMONIC_RET,      //          1001 0101 0000 1000
%	MNEMONIC_RETI,     //          1001 0101 0001 1000
%	MNEMONIC_SPM,      //          1001 0101 1110 1000
%	MNEMONIC_ESPM,     //          1001 0101 1111 1000
%	MNEMONIC_BREAK,    //          1001 0101 1001 1000
%	MNEMONIC_LPM,      //          1001 0101 1100 1000
%	MNEMONIC_ELPM,     //          1001 0101 1101 1000
%	MNEMONIC_BSET,     // s        1001 0100 0sss 1000
%	MNEMONIC_BCLR,     // s        1001 0100 1sss 1000
%	MNEMONIC_SER,      // Rd       1110 1111 dddd 1111
%	MNEMONIC_COM,      // Rd       1001 010d dddd 0000
%	MNEMONIC_NEG,      // Rd       1001 010d dddd 0001
%	MNEMONIC_INC,      // Rd       1001 010d dddd 0011
%	MNEMONIC_DEC,      // Rd       1001 010d dddd 1010
%	MNEMONIC_LSR,      // Rd       1001 010d dddd 0110
%	MNEMONIC_ROR,      // Rd       1001 010d dddd 0111
%	MNEMONIC_ASR,      // Rd       1001 010d dddd 0101
%	MNEMONIC_SWAP,     // Rd       1001 010d dddd 0010
%	MNEMONIC_PUSH,     // Rr       1001 001r rrrr 1111
%	MNEMONIC_POP,      // Rd       1001 000d dddd 1111
%	MNEMONIC_TST,      // Rd       0010 00dd dddd dddd
%	MNEMONIC_CLR,      // Rd       0010 01dd dddd dddd
%	MNEMONIC_LSL,      // Rd       0000 11dd dddd dddd
%	MNEMONIC_ROL,      // Rd       0001 11dd dddd dddd
%	MNEMONIC_BREQ,     // k        1111 00kk kkkk k001
%	MNEMONIC_BRNE,     // k        1111 01kk kkkk k001
%	MNEMONIC_BRCS,     // k        1111 00kk kkkk k000
%	MNEMONIC_BRCC,     // k        1111 01kk kkkk k000
%	MNEMONIC_BRSH,     // k        1111 01kk kkkk k000
%	MNEMONIC_BRLO,     // k        1111 00kk kkkk k000
%	MNEMONIC_BRMI,     // k        1111 00kk kkkk k010
%	MNEMONIC_BRPL,     // k        1111 01kk kkkk k010
%	MNEMONIC_BRGE,     // k        1111 01kk kkkk k100
%	MNEMONIC_BRLT,     // k        1111 00kk kkkk k100
%	MNEMONIC_BRHS,     // k        1111 00kk kkkk k101
%	MNEMONIC_BRHC,     // k        1111 01kk kkkk k101
%	MNEMONIC_BRTS,     // k        1111 00kk kkkk k110
%	MNEMONIC_BRTC,     // k        1111 01kk kkkk k110
%	MNEMONIC_BRVS,     // k        1111 00kk kkkk k011
%	MNEMONIC_BRVC,     // k        1111 01kk kkkk k011
%	MNEMONIC_BRIE,     // k        1111 00kk kkkk k111
%	MNEMONIC_BRID,     // k        1111 01kk kkkk k111
%	MNEMONIC_RJMP,     // k        1100 kkkk kkkk kkkk
%	MNEMONIC_RCALL,    // k        1101 kkkk kkkk kkkk
%	MNEMONIC_JMP,      // k        1001 010k kkkk 110k + 16k
%	MNEMONIC_CALL,     // k        1001 010k kkkk 111k + 16k
%	MNEMONIC_BRBS,     // s, k     1111 00kk kkkk ksss
%	MNEMONIC_BRBC,     // s, k     1111 01kk kkkk ksss
%	MNEMONIC_ADD,      // Rd, Rr   0000 11rd dddd rrrr
%	MNEMONIC_ADC,      // Rd, Rr   0001 11rd dddd rrrr
%	MNEMONIC_SUB,      // Rd, Rr   0001 10rd dddd rrrr
%	MNEMONIC_SBC,      // Rd, Rr   0000 10rd dddd rrrr
%	MNEMONIC_AND,      // Rd, Rr   0010 00rd dddd rrrr
%	MNEMONIC_OR,       // Rd, Rr   0010 10rd dddd rrrr
%	MNEMONIC_EOR,      // Rd, Rr   0010 01rd dddd rrrr
%	MNEMONIC_CP,       // Rd, Rr   0001 01rd dddd rrrr
%	MNEMONIC_CPC,      // Rd, Rr   0000 01rd dddd rrrr
%	MNEMONIC_CPSE,     // Rd, Rr   0001 00rd dddd rrrr
%	MNEMONIC_MOV,      // Rd, Rr   0010 11rd dddd rrrr
%	MNEMONIC_MUL,      // Rd, Rr   1001 11rd dddd rrrr
%	MNEMONIC_MOVW,     // Rd, Rr   0000 0001 dddd rrrr
%	MNEMONIC_MULS,     // Rd, Rr   0000 0010 dddd rrrr
%	MNEMONIC_MULSU,    // Rd, Rr   0000 0011 0ddd 0rrr
%	MNEMONIC_FMUL,     // Rd, Rr   0000 0011 0ddd 1rrr
%	MNEMONIC_FMULS,    // Rd, Rr   0000 0011 1ddd 0rrr
%	MNEMONIC_FMULSU,   // Rd, Rr   0000 0011 1ddd 1rrr
%	MNEMONIC_ADIW,     // Rd, K    1001 0110 KKdd KKKK
%	MNEMONIC_SBIW,     // Rd, K    1001 0111 KKdd KKKK
%	MNEMONIC_SUBI,     // Rd, K    0101 KKKK dddd KKKK
%	MNEMONIC_SBCI,     // Rd, K    0100 KKKK dddd KKKK
%	MNEMONIC_ANDI,     // Rd, K    0111 KKKK dddd KKKK
%	MNEMONIC_ORI,      // Rd, K    0110 KKKK dddd KKKK
%	MNEMONIC_SBR,      // Rd, K    0110 KKKK dddd KKKK
%	MNEMONIC_CPI,      // Rd, K    0011 KKKK dddd KKKK
%	MNEMONIC_LDI,      // Rd, K    1110 KKKK dddd KKKK
%	MNEMONIC_CBR,      // Rd, K    0111 KKKK dddd KKKK ~K
%	MNEMONIC_SBRC,     // Rr, b    1111 110r rrrr 0bbb
%	MNEMONIC_SBRS,     // Rr, b    1111 111r rrrr 0bbb
%	MNEMONIC_BST,      // Rr, b    1111 101d dddd 0bbb
%	MNEMONIC_BLD,      // Rd, b    1111 100d dddd 0bbb
%	MNEMONIC_IN,       // Rd, P    1011 0PPd dddd PPPP
%	MNEMONIC_OUT,      // P, Rr    1011 1PPr rrrr PPPP
%	MNEMONIC_SBIC,     // P, b     1001 1001 PPPP Pbbb
%	MNEMONIC_SBIS,     // P, b     1001 1011 PPPP Pbbb
%	MNEMONIC_SBI,      // P, b     1001 1010 PPPP Pbbb
%	MNEMONIC_CBI,      // P, b     1001 1000 PPPP Pbbb
%	MNEMONIC_LDS,      // Rd, k    1001 000d dddd 0000 + 16k
%	MNEMONIC_STS,      // k, Rr    1001 001d dddd 0000 + 16k
%	MNEMONIC_LD,       // Rd, __   dummy
%	MNEMONIC_ST,       // __, Rr   dummy
%	MNEMONIC_LDD,      // Rd, _+q  dummy
%	MNEMONIC_STD,      // _+q, Rr  dummy
%	MNEMONIC_COUNT,
%	MNEMONIC_LPM_Z,    // Rd, Z    1001 000d dddd 0100
%	MNEMONIC_LPM_ZP,   // Rd, Z+   1001 000d dddd 0101
%	MNEMONIC_ELPM_Z,   // Rd, Z    1001 000d dddd 0110
%	MNEMONIC_ELPM_ZP,  // Rd, Z+   1001 000d dddd 0111
%	MNEMONIC_LD_X,     // Rd, X    1001 000d dddd 1100
%	MNEMONIC_LD_XP,    // Rd, X+   1001 000d dddd 1101
%	MNEMONIC_LD_MX,    // Rd, -X   1001 000d dddd 1110
%	MNEMONIC_LD_Y,     // Rd, Y    1000 000d dddd 1000
%	MNEMONIC_LD_YP,    // Rd, Y+   1001 000d dddd 1001
%	MNEMONIC_LD_MY,    // Rd, -Y   1001 000d dddd 1010
%	MNEMONIC_LD_Z,     // Rd, Z    1000 000d dddd 0000
%	MNEMONIC_LD_ZP,    // Rd, Z+   1001 000d dddd 0001
%	MNEMONIC_LD_MZ,    // Rd, -Z   1001 000d dddd 0010
%	MNEMONIC_ST_X,     // X, Rr    1001 001d dddd 1100
%	MNEMONIC_ST_XP,    // X+, Rr   1001 001d dddd 1101
%	MNEMONIC_ST_MX,    // -X, Rr   1001 001d dddd 1110
%	MNEMONIC_ST_Y,     // Y, Rr    1000 001d dddd 1000
%	MNEMONIC_ST_YP,    // Y+, Rr   1001 001d dddd 1001
%	MNEMONIC_ST_MY,    // -Y, Rr   1001 001d dddd 1010
%	MNEMONIC_ST_Z,     // Z, Rr    1000 001d dddd 0000
%	MNEMONIC_ST_ZP,    // Z+, Rr   1001 001d dddd 0001
%	MNEMONIC_ST_MZ,    // -Z, Rr   1001 001d dddd 0010
%	MNEMONIC_LDD_Y,    // Rd, Y+q  10q0 qq0d dddd 1qqq
%	MNEMONIC_LDD_Z,    // Rd, Z+q  10q0 qq0d dddd 0qqq
%	MNEMONIC_STD_Y,    // Y+q, Rr  10q0 qq1r rrrr 1qqq
%	MNEMONIC_STD_Z,    // Z+q, Rr  10q0 qq1r rrrr 0qqq

code({M}) ->
    case M of
	nop ->
	    <<2#0000000000000000:16>>;
	sec ->
	    <<2#1001010000001000:16>>
    end;
code({M, S, K}) ->
    case M of
	brbs -> <<2#111100:6,K:7,S:3>>;
	brbc -> <<2#111101:6,K:7,S:3>>
    end.


%%% Addressing

register_addr(RegAddr) ->
    F = fun(N) -> {list_to_atom([$r|integer_to_list(N)]), N} end,
    Regs = dict:from_list(lists:map(F, lists:seq(0, 31))),
    Has = dict:is_key(RegAddr, Regs),
    if
	Has ->
	    dict:fetch(RegAddr, Regs);
	is_integer(RegAddr) ->
	    RegAddr;
	true ->
	    throw({badarg, RegAddr})
    end.

pc_relative_addr(ProgramAddr, PC) ->
    ProgramAddr - PC - 1.


%%% Labels

labels_new() ->
    dict:new().

labels_add(Name, Addr, Labels) ->
    dict:store(Name, Addr, Labels).

labels_fetch(Name, Labels) ->
    dict:fetch(Name, Labels).


%%% First pass

pass_1(A, L) ->
    pass_1(A, labels_new(), [], L).

pass_1(_, Labels, Passed, []) ->
    {Labels, lists:reverse(Passed)};
pass_1(_, Labels, Passed, [{org,Address}|T]) ->
    pass_1(Address, Labels, Passed, T);
pass_1(A, Labels, Passed, [{label,Name}|T]) ->
    pass_1(A, labels_add(Name, A, Labels), Passed, T);
pass_1(A, Labels, Passed, [H|T]) ->
    [Name|Operands] = tuple_to_list(H),
    {Size, _} = instruction(Name),
    pass_1(A + Size, Labels, [{A, Name, list_to_tuple(Operands)}|Passed], T).


%%% Second pass

pass_2(Labels, {A, M, Operands}) ->
    {_, F} = instruction(M),
    {A, M, F(Labels, A, Operands)};
pass_2(_, []) ->
    [];
pass_2(Labels, [H|T]) ->
    [pass_2(Labels, H)|pass_2(Labels, T)].

asm(Instructions) ->
    {Labels, FirstPassed} = pass_1(0, labels_new(), [], Instructions),
    pass_2(Labels, FirstPassed).


%%% Tests

nop_test() ->
    code({nop}) =:= <<2#0000000000000000:16>>.

sec_test() ->
    code({sec}) =:= <<2#1001010000001000:16>>.

brbs_test() ->
    code({brbs, 1, 63}) =:= <<2#1111001111111001>>.

brbc_test() ->
    code({brbs, 7, 1}) =:= <<2#1111000000001111>>.

register_addr_test_() ->
    [?_assert(0 =:= register_addr(r0)),
     ?_assert(1 =:= register_addr(1))].

register_addr_badarg_test() ->
    ?assertThrow({badarg, r32}, register_addr(r32)),
    ?assertThrow({badarg, ab}, register_addr(ab)).

pc_relative_addr_test() ->
    ?assert(0 == pc_relative_addr(5, 4)).

pass_1_label_test() ->
    {S, Passed} = pass_1(0, [{nop}, {label, l1}]),
    ?assert(1 == labels_fetch(l1, S)),
    ?assertMatch([{0, nop, {}}], Passed).

pass_1_org_test() ->
    ?assertMatch({_, [{4, nop, {}}]}, pass_1(0, [{org, 4}, {nop}])).

pass_1_test_() ->
    [?_assertMatch({_, [{0, nop, {}}, {1, nop, {}}]}, pass_1(0, [{nop}, {nop}])),
     ?_assertMatch({_, [{0, bclr, {7}}, {1, nop, {}}]}, pass_1(0, [{bclr, 7}, {nop}]))].

pass_2_test_() ->
    [?_assertMatch([], pass_2(labels_new(), [])),
     ?_assertMatch([{3, nop, {}}], pass_2(labels_new(), [{3, nop, {}}]))].

pass_2_nop_test_() ->
    S = labels_new(),
    [?_assertMatch({0, nop, {}}, pass_2(S, {0, nop, {}}))].

pass_2_bset_test_() ->
    S = labels_new(),
    [?_assertMatch({3, bset, {0}}, pass_2(S, {3, bset, {0}}))].

pass_2_bclr_test_() ->
    S = labels_new(),
    [?_assertMatch({3, bclr, {7}}, pass_2(S, {3, bclr, {7}}))].

pass_2_ser_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, ser, {15}}, pass_2(S, {0, ser, {r15}}))
    ].

pass_2_com_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, com, {31}}, pass_2(S, {0, com, {r31}}))
    ].

pass_2_tst_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, tst, {31}}, pass_2(S, {0, tst, {r31}}))
    ].

pass_2_brbs_test_() ->
    S = labels_add(l1, 5, labels_new()),
    [?_assertMatch({3, brbs, {0, 1}}, pass_2(S, {3, brbs, {0, l1}}))].

pass_2_brbc_test_() ->
    S = labels_add(l1, 5, labels_new()),
    [?_assertMatch({3, brbc, {0, 1}}, pass_2(S, {3, brbc, {0, l1}}))].

pass_2_ldi_test() ->
    S = labels_new(),
    ?assertMatch({0, ldi, {1, 255}}, pass_2(S, {0, ldi, {r1, 255}})).

pass_2_dec_test() ->
    S = labels_new(),
    ?assertMatch({0, dec, {1}}, pass_2(S, {0, dec, {r1}})).

pass_2_brne_test() ->
    S = labels_add(l2, 5, labels_new()),
    ?assertMatch({0, brne, {4}}, pass_2(S, {0, brne, {l2}})).

pass_2_adc_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, adc, {0, 31}}, pass_2(S, {0, adc, {r0, r31}}))
    ].

pass_2_add_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, add, {0, 31}}, pass_2(S, {0, add, {r0, r31}}))
    ].

pass_2_adiw_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, adiw, {3, 63}}, pass_2(S, {0, adiw, {r30, 63}}))
    ].

pass_2_and_test_() ->
    S = labels_new(),
    [
     ?_assertMatch({0, and_, {31, 0}}, pass_2(S, {0, and_, {r31, r0}}))
    ].

asm_test() ->
    ?assertMatch([{0, nop, {}}], asm([{nop}])).

asm_brbc_test_() ->
    [?_assertMatch([{0, brbc, {0, -1}}], asm([{label, l1}, {brbc, 0, l1}])),
     ?_assertMatch([{0, brbc, {0, 0}}], asm([{brbc, 0, l1}, {label, l1}])),
     ?_assertMatch([{100, brbc, {0, -1}}], asm([{org, 100}, {label, l1}, {brbc, 0, l1}]))].
