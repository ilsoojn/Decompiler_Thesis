define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = load i64, i64* %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = trunc i64 %RIP_init to i32
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
%RBP_init = load i64, i64* %RBP_ptr
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
%RSP_init = load i64, i64* %RSP_ptr
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = trunc i64 %RSP_init to i32
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = trunc i64 %RBP_init to i32
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = load i64, i64* %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = trunc i64 %RAX_init to i32
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%1 = lshr i64 %RAX_init, 8
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%2 = %ZMM0_init
%3 = %ZMM0_init
%XMM0_init = bitcast i128 %ZMM0_init to <4 x float>
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%4 = %ZMM0_init
%5 = %ZMM0_init
%YMM0_init = bitcast i256 %ZMM0_init to <8 x float>
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = load <16 x float>, <16 x float>* %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%6 = %ZMM1_init
%7 = %ZMM1_init
%XMM1_init = bitcast i128 %ZMM1_init to <4 x float>
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %ZMM1_init
%9 = %ZMM1_init
%YMM1_init = bitcast i256 %ZMM1_init to <8 x float>
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = load i64, i64* %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = trunc i64 %RCX_init to i32
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %RCX_init, 8
br label %bb_400480
exit_fn_400480:                                   ; preds = %bb_400480
%13 = [%RAX]
store i64 %13, i64* %RAX_ptr
%14 = [%RBP]
store i64 %14, i64* %RBP_ptr
%15 = [%RCX]
store i64 %15, i64* %RCX_ptr
%16 = [%RIP]
store i64 %16, i64* %RIP_ptr
%17 = [%RSP]
store i64 %17, i64* %RSP_ptr
%18 = [%ZMM0]
store <16 x float> %18, <16 x float>* %ZMM0_ptr
%19 = [%ZMM1]
store <16 x float> %19, <16 x float>* %ZMM1_ptr
ret void
bb_400480:                                        ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%21 = %RSP-8
store i64 %RBP, i64* %RSP-8, align 1
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
%25 = [4195672]
%26 = %25
%ZMM0_0 = %ZMM0
%27 = %ZMM0
%XMM0_0 = %ZMM0
%XMM0_1 = %25
%30 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %25
%33 = %ZMM0
%ZMM0_1 = %25
%RIP_5 = 4195478
%EIP_4 = 4195478
%37 = 4195680
%38 = [4195680]
%39 = %38
%ZMM1_0 = %ZMM1
%40 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = %38
%43 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = %38
%46 = %ZMM1
%ZMM1_1 = %38
%RIP_6 = 4195485
%EIP_5 = 4195485
%50 = %RSP-12
store i32 0, i32* %RSP-12, align 1
%RIP_7 = 4195492
%EIP_6 = 4195492
%52 = %RSP-16
store i32 10, i32* %RSP-16, align 1
%RIP_8 = 4195497
%EIP_7 = 4195497
%53 = %38
%54 = %38
%56 = %RSP-24
store double %38, double* %RSP-24, align 1
%RIP_9 = 4195502
%EIP_8 = 4195502
%57 = %25
%58 = %25
%60 = %RSP-32
store double %25, double* %RSP-32, align 1
%RIP_10 = 4195505
%EIP_9 = 4195505
%62 = %RSP-16
%ECX_0 = [%RSP-16]
%RCX_0 = %RCX
%RCX_1 = %RSP-16
%63 = lshr i32 [%RSP-16], 8
%RIP_11 = 4195509
%EIP_10 = 4195509
%64 = [%RSP-16]
%65 = [%RSP-16]
%XMM0_2 = [%RSP-16]
%YMM0_2 = [%RSP-16]
%ZMM0_2 = [%RSP-16]
%RIP_12 = 4195514
%EIP_11 = 4195514
%72 = [%RSP-16]
%73 = [%RSP-16]
%75 = %RSP-32
%76 = [%RSP-32]
%77 = fmul double [%RSP-16], %76
%78 = %77
%XMM0_3 = %77
%YMM0_3 = %77
%ZMM0_3 = %77
%RIP_13 = 4195519
%EIP_12 = 4195519
%85 = %77
%86 = %77
%88 = %RSP-24
%89 = [%RSP-24]
%90 = fadd double %77, %89
%91 = %90
%XMM0_4 = %90
%YMM0_4 = %90
%ZMM0_4 = %90
%RIP_14 = 4195524
%EIP_13 = 4195524
%98 = %90
%99 = %90
%101 = %RSP-40
store double %90, double* %RSP-40, align 1
%RIP_15 = 4195525
%EIP_14 = 4195525
%RSP_2 = %RSP
%ESP_1 = %RSP
%103 = %RSP-8
%RBP_1 = [%RSP+8]
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
store i32 %RSP+8, i32* %EBP
store i32 [%RSP-16], i32* %ECX
store i32 %RSP, i32* %EIP
store i32 %RSP+8, i32* %ESP
store i64 %EAX_1, i64* %RAX
store i64 [%RSP+8], i64* %RBP
store i64 %RSP-16, i64* %RCX
store i64 %RSP, i64* %RIP
store i64 %RSP_3, i64* %RSP
%125 = %90
store <4 x float> %90, <4 x float>* %XMM0
%126 = %38
store <4 x float> %38, <4 x float>* %XMM1
%127 = %90
store <8 x float> %90, <8 x float>* %YMM0
%128 = %38
store <8 x float> %38, <8 x float>* %YMM1
%129 = %90
store <16 x float> %90, <16 x float>* %ZMM0
%130 = %38
store <16 x float> %38, <16 x float>* %ZMM1
br label %exit_fn_400480
}

