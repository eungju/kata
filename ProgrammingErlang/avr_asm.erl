-module(avr_asm).
-export([asm/1]).
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_unused_vars).

operand_none(Labels, A, {}) ->
    {}.

operand_Rd5_Rr5(Labels, A, {Rd5, Rr5}) ->
    {register_addr(Rd5), register_addr(Rr5)}.

operand_Rd2_K6(Labels, A, {Rd2, K6}) ->
    {(register_addr(Rd2) - 24) div 2, K6}.

operand_Rd5_K8(Labels, A, {Rd5, K8}) ->
    {register_addr(Rd5), K8}.

operand_Rd4(Labels, A, {Rd4}) ->
    {register_addr(Rd4) - 16}.

operand_Rd5(Labels, A, {Rd5}) ->
    {register_addr(Rd5)}.

operand_Rd5Rd5(Labels, A, {Rd5}) ->
    operand_Rd5_Rr5(Labels, A, {Rd5, Rd5}).

operand_Rd5_b3(Labels, A, {Rd5, B3}) ->
    {register_addr(Rd5), B3}.

operand_s3(Labels, A, {S3}) ->
    {S3}.

operand_s3_k7(Labels, A, {S3, K6}) ->
    {S3, pc_relative_addr(labels_fetch(K6, Labels), A)}.

operand_Rd4_K8(Labels, A, {Rd4, K8}) ->
    {register_addr(Rd4) - 16, K8}.

operand_k7(Labels, A, {K7}) ->
    {pc_relative_addr(labels_fetch(K7, Labels), A)}.

operand_k22(Labels, A, {K22}) ->
    {labels_fetch(K22, Labels)}.

operand_A5_b3(Labels, A, {A5, B3}) ->
    {A5, B3}.

operand_Rd3_Rr3(Labels, A, {Rd3, Rr3}) ->
    {register_addr(Rd3) - 16, register_addr(Rr3) - 16}.

operand_Rd5_A6(Labels, A, {Rd5, A6}) ->
    {register_addr(Rd5), A6}.

operand_Rd5_k16(Labels, A, {Rd5, K16}) ->
    {register_addr(Rd5), K16}.

operand_2Rd4_2Rr4(Labels, A, {Rd4, Rr4}) ->
    {register_addr(Rd4) div 2, register_addr(Rr4) div 2}.

operand_Rd4_Rr4(Labels, A, {Rd4, Rr4}) ->
    {register_addr(Rd4) - 16, register_addr(Rr4) - 16}.

operand_A6_Rr5(Labels, A, {A6, Rr5}) ->
    {A6, register_addr(Rr5)}.

operand_Rr5(Labels, A, {Rr5}) ->
    operand_Rd5(Labels, A, {Rr5}).

operand_k12(Labels, A, {K12}) ->
    {pc_relative_addr(labels_fetch(K12, Labels), A)}.

operand_Rr5_b3(Labels, A, {Rr5, B3}) ->
    operand_Rd5_b3(Labels, A, {Rr5, B3}).

operand_Rr5_q5(Labels, A, {Rr5, Q5}) ->
    {register_addr(Rr5), Q5}.

operand_none_test() ->
    ?assertMatch({}, operand_none(labels_new(), 0, {})).

operand_Rd5_Rr5_test() ->
    ?assertMatch({31, 31}, operand_Rd5_Rr5(labels_new(), 0, {r31, r31})).

operand_Rd2_K6_test() ->
    ?assertMatch({3, 63}, operand_Rd2_K6(labels_new(), 0, {r30, 63})).

operand_Rd5_K8_test() ->
    ?assertMatch({31, 255}, operand_Rd5_K8(labels_new(), 0, {r31, 255})).

operand_Rd4_test() ->
    ?assertMatch({15}, operand_Rd4(labels_new(), 0, {r31})).

operand_Rd5_test() ->
    ?assertMatch({31}, operand_Rd5(labels_new(), 0, {r31})).

operand_Rd5Rd5_test() ->
    ?assertMatch({31, 31}, operand_Rd5Rd5(labels_new(), 0, {r31})).

operand_Rd5_b3_test() ->
    ?assertMatch({31, 7}, operand_Rd5_b3(labels_new(), 0, {r31, 7})).

operand_s3_test() ->
    ?assertMatch({7}, operand_s3(labels_new(), 0, {7})).

operand_s3_k7_test() ->
    Labels = labels_add(l1, 64, labels_new()),
    ?assertMatch({7, 63}, operand_s3_k7(Labels, 0, {7, l1})).

operand_Rd4_K8_test() ->
    ?assertMatch({15, 255}, operand_Rd4_K8(labels_new(), 0, {r31, 255})).

operand_k7_test() ->
    Labels = labels_add(l1, 0, labels_new()),
    ?assertMatch({-64}, operand_k7(Labels, 63, {l1})).

operand_k22_test() ->
    Labels = labels_add(l1, 4 * 1024 * 1024, labels_new()),
    ?assertMatch({4 * 1024 * 1024}, operand_k22(Labels, 0, {l1})).

operand_A5_b3_test() ->
    ?assertMatch({31, 7}, operand_A5_b3(labels_new(), 0, {31, 7})).

operand_Rd3_Rr3_test() ->
    ?_assertMatch({7, 7}, operand_Rd3_Rr3(labels_new(), 0, {r23, r23})).

operand_Rd5_A6_test() ->
    ?_assertMatch({31, 63}, operand_Rd5_A6(labels_new(), 0, {r31, 63})).

operand_Rd5_k16_test() ->
    ?_assertMatch({31, 65535}, operand_Rd5_k16(labels_new(), 0, {r31, 65535})).

operand_2Rd4_2Rr4_test() ->
    ?_assertMatch({1, 15}, operand_2Rd4_2Rr4(labels_new(), 0, {r2, r30})).

operand_Rd4_Rr4_test() ->
    ?_assertMatch({0, 15}, operand_Rd4_Rr4(labels_new(), 0, {r16, r31})).

operand_A6_Rr5_test() ->
    ?_assertMatch({63, 31}, operand_A6_Rr5(labels_new(), 0, {63, r31})).

operand_Rr5_test() ->
    ?assertMatch({31}, operand_Rr5(labels_new(), 0, {r31})).

operand_k12_test() ->
    Labels = labels_add(l1, 2048, labels_new()),
    ?assertMatch({2047}, operand_k12(Labels, 0, {l1})).

operand_Rr5_b3_test() ->
    ?assertMatch({31, 7}, operand_Rr5_b3(labels_new(), 0, {r31, 7})).

operand_Rr5_q5_test() ->
    ?assertMatch({31, 63}, operand_Rr5_q5(labels_new(), 0, {r31, 63})).

instruction(Name) ->
    case Name of
	adc ->
	    {1, fun operand_Rd5_Rr5/3};
	add ->
	    {1, fun operand_Rd5_Rr5/3};
	adiw ->
	    {1, fun operand_Rd2_K6/3};
	and_ ->
	    {1, fun operand_Rd5_Rr5/3};
	andi_ ->
	    {1, fun operand_Rd5_K8/3};
	asr_ ->
	    {1, fun operand_Rd5/3};
	bclr ->
	    {1, fun operand_s3/3};
	bld_ ->
	    {1, fun operand_Rd5_b3/3};
	brbc ->
	    {1, fun operand_s3_k7/3};
	brbs ->
	    {1, fun operand_s3_k7/3};
	brcc ->
	    {1, fun operand_k7/3};
	brcs ->
	    {1, fun operand_k7/3};
	break ->
	    {1, fun operand_none/3};
	breq ->
	    {1, fun operand_k7/3};
	brge ->
	    {1, fun operand_k7/3};
	brhc ->
	    {1, fun operand_k7/3};
	brhs ->
	    {1, fun operand_k7/3};
	brid ->
	    {1, fun operand_k7/3};
	brie ->
	    {1, fun operand_k7/3};
	brlo ->
	    {1, fun operand_k7/3};
	brlt ->
	    {1, fun operand_k7/3};
	brmi ->
	    {1, fun operand_k7/3};
	brne ->
	    {1, fun operand_k7/3};
	brpl ->
	    {1, fun operand_k7/3};
	brsh ->
	    {1, fun operand_k7/3};
	brtc ->
	    {1, fun operand_k7/3};
	brts ->
	    {1, fun operand_k7/3};
	brvc ->
	    {1, fun operand_k7/3};
	brvs ->
	    {1, fun operand_k7/3};
	bset ->
	    {1, fun operand_s3/3};
	bst ->
	    {1, fun operand_Rd5_b3/3};
        call ->
	    {2, fun operand_k22/3};
        cbi ->
	    {2, fun operand_A5_b3/3};
        cbr ->
	    {2, fun operand_Rd4_K8/3};
        clc ->
	    {2, fun operand_none/3};
        clh ->
	    {2, fun operand_none/3};
        cli ->
	    {2, fun operand_none/3};
        cln ->
	    {2, fun operand_none/3};
        clr ->
	    {2, fun operand_Rd5Rd5/3};
        cls ->
	    {2, fun operand_none/3};
        clt ->
	    {2, fun operand_none/3};
        clv ->
	    {2, fun operand_none/3};
        clz ->
	    {2, fun operand_none/3};
	com ->
	    {1, fun operand_Rd5/3};
	cp ->
	    {1, fun operand_Rd5_Rr5/3};
	cpc ->
	    {1, fun operand_Rd5_Rr5/3};
	cpi ->
	    {1, fun operand_Rd4_K8/3};
	cpse ->
	    {1, fun operand_Rd5_Rr5/3};
	dec ->
	    {1, fun operand_Rd5/3};
	eicall ->
	    {1, fun operand_none/3};
	eijmp ->
	    {1, fun operand_none/3};
	elpm ->
	    {1, fun operand_none/3};
	elpmz ->
	    {1, fun operand_Rd5/3};
	elpmzi ->
	    {1, fun operand_Rd5/3};
	eor ->
	    {1, fun operand_Rd5_Rr5/3};
	fmul ->
	    {1, fun operand_Rd3_Rr3/3};
	fmuls ->
	    {1, fun operand_Rd3_Rr3/3};
	fmulsu ->
	    {1, fun operand_Rd3_Rr3/3};
	icall ->
	    {1, fun operand_none/3};
	ijmp ->
	    {1, fun operand_none/3};
	in ->
	    {1, fun operand_Rd5_A6/3};
	inc ->
	    {1, fun operand_Rd5/3};
	jmp ->
	    {1, fun operand_k22/3};
	ldx ->
	    {1, fun operand_Rd5/3};
	ldxi ->
	    {1, fun operand_Rd5/3};
	ldxd ->
	    {1, fun operand_Rd5/3};
	ldy ->
	    {1, fun operand_Rd5/3};
	ldyi ->
	    {1, fun operand_Rd5/3};
	ldyd ->
	    {1, fun operand_Rd5/3};
	ldz ->
	    {1, fun operand_Rd5/3};
	ldzi ->
	    {1, fun operand_Rd5/3};
	ldzd ->
	    {1, fun operand_Rd5/3};
	ldi ->
	    {1, fun operand_Rd4_K8/3};
	lds ->
	    {2, fun operand_Rd5_k16/3};
	lpm ->
	    {1, fun operand_none/3};
	lpmz ->
	    {1, fun operand_Rd5/3};
	lpmzi ->
	    {1, fun operand_Rd5/3};
	lsl ->
	    {1, fun operand_Rd5/3};
	lsr ->
	    {1, fun operand_Rd5/3};
	mov ->
	    {1, fun operand_Rd5_Rr5/3};
	movw ->
	    {1, fun operand_2Rd4_2Rr4/3};
	mul ->
	    {1, fun operand_Rd5_Rr5/3};
	muls ->
	    {1, fun operand_Rd4_Rr4/3};
	mulsu ->
	    {1, fun operand_Rd3_Rr3/3};       
	neg ->
	    {1, fun operand_Rd5/3};
	nop ->
	    {1, fun operand_none/3};
	or_ ->
	    {1, fun operand_Rd5_Rr5/3};
	ori_ ->
	    {1, fun operand_Rd4_K8/3};
	out_ ->
	    {1, fun operand_A6_Rr5/3};
	pop_ ->
	    {1, fun operand_Rd5/3};
	push_ ->
	    {1, fun operand_Rr5/3};
	rcall_ ->
	    {1, fun operand_k12/3};
	ret_ ->
	    {1, fun operand_none/3};
	reti_ ->
	    {1, fun operand_none/3};
	rjmp_ ->
	    {1, fun operand_k12/3};
	rol_ ->
	    {1, fun operand_Rd5/3};
	ror_ ->
	    {1, fun operand_Rd5/3};
	sbc_ ->
	    {1, fun operand_Rd5_Rr5/3};
	sbci_ ->
	    {1, fun operand_Rd4_K8/3};
	sbi_ ->
	    {1, fun operand_A5_b3/3};
	sbic_ ->
	    {1, fun operand_A5_b3/3};
	sbis_ ->
	    {1, fun operand_A5_b3/3};
	sbiw_ ->
	    {1, fun operand_Rd2_K6/3};
	sbr_ ->
	    {1, fun operand_Rd4_K8/3};
	sbrc_ ->
	    {1, fun operand_Rr5_b3/3};
	sbrs_ ->
	    {1, fun operand_Rr5_b3/3};
	sec ->
	    {1, fun operand_none/3};
	seh ->
	    {1, fun operand_none/3};
	sei ->
	    {1, fun operand_none/3};
	sen ->
	    {1, fun operand_none/3};
	ser ->
	    {1, fun operand_Rd4/3};
	ses ->
	    {1, fun operand_none/3};
	set ->
	    {1, fun operand_none/3};
	sev ->
	    {1, fun operand_none/3};
	sez ->
	    {1, fun operand_none/3};
	sleep ->
	    {1, fun operand_none/3};
	spm ->
	    {1, fun operand_none/3};
	stx ->
	    {1, fun operand_Rr5/3};
	stxi ->
	    {1, fun operand_Rr5/3};
	stxd ->
	    {1, fun operand_Rr5/3};
	sty ->
	    {1, fun operand_Rr5/3};
	styi ->
	    {1, fun operand_Rr5/3};
	styd ->
	    {1, fun operand_Rr5/3};
	stdy ->
	    {1, fun operand_Rr5_q5/3};
	stz ->
	    {1, fun operand_Rr5/3};
	stzi ->
	    {1, fun operand_Rr5/3};
	stzd ->
	    {1, fun operand_Rr5/3};
	stdz ->
	    {1, fun operand_Rr5_q5/3};

	tst ->
	    {1, fun operand_Rd5Rd5/3};
	_ ->
	    throw({badarg, Name})
    end.

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

instruction_test() ->
    ?assertThrow({badarg, unknown}, instruction(unknown)).

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

asm_test() ->
    ?assertMatch([{0, nop, {}}], asm([{nop}])).

asm_brbc_test_() ->
    [?_assertMatch([{0, brbc, {0, -1}}], asm([{label, l1}, {brbc, 0, l1}])),
     ?_assertMatch([{0, brbc, {0, 0}}], asm([{brbc, 0, l1}, {label, l1}])),
     ?_assertMatch([{100, brbc, {0, -1}}], asm([{org, 100}, {label, l1}, {brbc, 0, l1}]))].
