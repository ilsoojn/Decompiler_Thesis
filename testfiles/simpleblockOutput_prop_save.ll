define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %regset*
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %regset*
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
%RBP_init = %regset*
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
%RSP_init = %regset*
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = %regset*
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = %regset*
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %regset*
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %regset*
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%1 = lshr i64 %regset*, 8
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = %regset*
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%2 = %regset*
%3 = %2
%XMM0_init = %regset*
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%4 = %regset*
%5 = %4
%YMM0_init = %regset*
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %regset*
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%6 = %regset*
%7 = %6
%XMM1_init = %regset*
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %regset*
%9 = %8
%YMM1_init = %regset*
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %regset*
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %regset*
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %regset*, 8
br label %bb_400480
exit_fn_400480:                                   ; preds = %bb_400480
%13 = load i64, i64* %RAX
store i64 %13, i64* %RAX_ptr
%14 = load i64, i64* %RBP
store i64 %14, i64* %RBP_ptr
%15 = load i64, i64* %RCX
store i64 %15, i64* %RCX_ptr
%16 = load i64, i64* %RIP
store i64 %16, i64* %RIP_ptr
%17 = load i64, i64* %RSP
store i64 %17, i64* %RSP_ptr
%18 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %18, <16 x float>* %ZMM0_ptr
%19 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %19, <16 x float>* %ZMM1_ptr
ret void
bb_400480:                                        ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%21 = %RSP-8
store i64 %RBP_0, i64* %21, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195460
%EIP_1 = 4195460
%EBP_0 = %RSP-8
%RIP_3 = 4195462
%EIP_2 = 4195462
%RAX_0 = %RAX
%EAX_0 = %RAX
%EAX_1 = xor i32 %EAX_0, %EAX_0
%RAX_1 = %EAX_1
%22 = lshr i32 %EAX_1, 8
%RIP_4 = 4195470
%EIP_3 = 4195470
%24 = 4195672
%25 = load double, double* %24, align 1
%26 = %25
%ZMM0_0 = %ZMM0
%27 = %ZMM0
%XMM0_0 = %ZMM0
%XMM0_1 = %XMM0_0 : %25
%30 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %YMM0_0 : %XMM0_1
%33 = %ZMM0
%ZMM0_1 = %33 : %XMM0_1
%RIP_5 = 4195478
%EIP_4 = 4195478
%37 = 4195680
%38 = load double, double* %37, align 1
%39 = %38
%ZMM1_0 = %ZMM1
%40 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = %XMM1_0 : %38
%43 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = %YMM1_0 : %XMM1_1
%46 = %ZMM1
%ZMM1_1 = %46 : %XMM1_1
%RIP_6 = 4195485
%EIP_5 = 4195485
%50 = %RSP-12
store i32 0, i32* %50, align 1
%RIP_7 = 4195492
%EIP_6 = 4195492
%52 = %RSP-16
store i32 10, i32* %52, align 1
%RIP_8 = 4195497
%EIP_7 = 4195497
%53 = %XMM1_0
%54 = %38
%56 = %RSP-24
store double %38, double* %56, align 1
%RIP_9 = 4195502
%EIP_8 = 4195502
%57 = %XMM0_0
%58 = %25
%60 = %RSP-32
store double %25, double* %60, align 1
%RIP_10 = 4195505
%EIP_9 = 4195505
%62 = %RSP-16
%ECX_0 = [ %RSP-16 ]
%RCX_0 = %RCX
%RCX_1 = %RSP-16
%63 = lshr i32 [ %RSP-16 ], 8
%RIP_11 = 4195509
%EIP_10 = 4195509
%64 = [ %RSP-16 ]
%65 = %64
%XMM0_2 = %XMM0_1 : %64
%YMM0_2 = %YMM0_1 : %XMM0_2
%ZMM0_2 = %ZMM0_1 : %XMM0_2
%RIP_12 = 4195514
%EIP_11 = 4195514
%72 = %XMM0_1
%73 = %64
%75 = %RSP-32
%76 = load double, double* %75, align 1
%77 = fmul double %64, %76
%78 = %77
%XMM0_3 = %XMM0_2 : %77
%YMM0_3 = %YMM0_2 : %XMM0_3
%ZMM0_3 = %ZMM0_2 : %XMM0_3
%RIP_13 = 4195519
%EIP_12 = 4195519
%85 = %XMM0_2
%86 = %77
%88 = %RSP-24
%89 = load double, double* %88, align 1
%90 = fadd double %77, %89
%91 = %90
%XMM0_4 = %XMM0_3 : %90
%YMM0_4 = %YMM0_3 : %XMM0_4
%ZMM0_4 = %ZMM0_3 : %XMM0_4
%RIP_14 = 4195524
%EIP_13 = 4195524
%98 = %XMM0_3
%99 = %90
%101 = %RSP-40
store double %90, double* %101, align 1
%RIP_15 = 4195525
%EIP_14 = 4195525
%RSP_2 = %RSP
%ESP_1 = %RSP
%103 = %RSP-8
%RBP_1 = [ %RSP+8 ]
%EBP_1 = %RSP+8
%RIP_16 = 4195526
%EIP_15 = 4195526
%104 = %RSP+8
%RIP_17 = %RSP
%ESP_2 = %RSP+8
%EIP_16 = %RSP
%ZF_0 = icmp eq i32 %EAX_1, 0
%SF_0 = icmp slt i32 %EAX_1, 0
%105 = %EAX_1
%106 = call i8 @llvm.ctpop.i8(i8 %EAX_1)
%107 = %106
%PF_0 = icmp eq i1 %106, false
%108 = false
%109 = shl i32 false, 0
%111 = %PF_0
%112 = shl i32 %PF_0, 2
%114 = false
%115 = shl i32 false, 4
%117 = %ZF_0
%118 = shl i32 %ZF_0, 6
%120 = %SF_0
%121 = shl i32 %SF_0, 7
%123 = false
%124 = shl i32 false, 11
store i32 %EAX_1, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %ECX_0, i32* %ECX
store i32 %EIP_16, i32* %EIP
store i32 %ESP_2, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 %RCX_1, i64* %RCX
store i64 %RIP_17, i64* %RIP
store i64 %RSP_3, i64* %RSP
%125 = %XMM0_3
store <4 x float> %90, <4 x float>* %XMM0
%126 = %XMM1_0
store <4 x float> %38, <4 x float>* %XMM1
%127 = %YMM0_3
store <8 x float> %XMM0_4, <8 x float>* %YMM0
%128 = %YMM1_0
store <8 x float> %XMM1_1, <8 x float>* %YMM1
%129 = %ZMM0_3
store <16 x float> %XMM0_4, <16 x float>* %ZMM0
%130 = %46
store <16 x float> %XMM1_1, <16 x float>* %ZMM1
br label %exit_fn_400480
}

