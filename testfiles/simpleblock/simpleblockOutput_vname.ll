define void @fn_400480(%regset* noalias nocapture) {
%f = alloca double, align 8
%e = alloca double, align 8
%d = alloca double, align 8
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %RIP_ptr
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
%RBP_init = %RBP_ptr
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
%RSP_init = %RSP_ptr
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = %RSP_ptr
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = %RBP_ptr
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%1 = lshr i64 %RAX_init, 8
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%2 = %ZMM0_init
%3 = %ZMM0_init
%XMM0_init = %ZMM0_ptr
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%4 = %ZMM0_init
%5 = %ZMM0_init
%YMM0_init = %ZMM0_ptr
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%6 = %ZMM1_init
%7 = %ZMM1_init
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %ZMM1_init
%9 = %ZMM1_init
%YMM1_init = %ZMM1_ptr
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %RCX_ptr
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %RCX_init, 8
br label %bb_400480
exit_fn_400480: ; preds = %bb_400480
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
bb_400480: ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%21 = %a
store i64 %RBP, i64* %a, align 1
%RSP_1 = %a
%ESP_0 = %a
%RIP_2 = 4195460
%EIP_1 = 4195460
%EBP_0 = %a
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
%25 = load double, double* 4195672, align 1
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
%38 = load double, double* 4195680, align 1
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
%50 = %b
store i32 0, i32* %b, align 1
%RIP_7 = 4195492
%EIP_6 = 4195492
%52 = %c
store i32 10, i32* %c, align 1
%RIP_8 = 4195497
%EIP_7 = 4195497
%53 = %38
%54 = %38
%56 = %d
store double %38, double* %d, align 1
%RIP_9 = 4195502
%EIP_8 = 4195502
%57 = %25
%58 = %25
%60 = %e
store double %25, double* %e, align 1
%RIP_10 = 4195505
%EIP_9 = 4195505
%62 = %c
%ECX_0 = %c
%RCX_0 = %RCX
%RCX_1 = %c
%63 = lshr i32 %c, 8
%RIP_11 = 4195509
%EIP_10 = 4195509
%64 = %c
%65 = %c
%XMM0_2 = %c
%YMM0_2 = %c
%ZMM0_2 = %c
%RIP_12 = 4195514
%EIP_11 = 4195514
%72 = %c
%73 = %c
%75 = %e
%76 = load double, double* %e, align 1
%77 = fmul double %c, %76
%78 = %77
%XMM0_3 = %77
%YMM0_3 = %77
%ZMM0_3 = %77
%RIP_13 = 4195519
%EIP_12 = 4195519
%85 = %77
%86 = %77
%88 = %d
%89 = load double, double* %d, align 1
%90 = fadd double %77, %89
%91 = %90
%XMM0_4 = %90
%YMM0_4 = %90
%ZMM0_4 = %90
%RIP_14 = 4195524
%EIP_13 = 4195524
%98 = %90
%99 = %90
%101 = %f
store double %90, double* %f, align 1
%RIP_15 = 4195525
%EIP_14 = 4195525
%RSP_2 = %RSP
%ESP_1 = %RSP
%103 = %a
%RBP_1 = %a
%EBP_1 = %a
%RIP_16 = 4195526
%EIP_15 = 4195526
%RSP_3 = %RSP+8
%104 = %RSP
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
store i32 %a, i32* %EBP
store i32 %c, i32* %ECX
store i32 %RSP, i32* %EIP
store i32 %RSP+8, i32* %ESP
store i64 %EAX_1, i64* %RAX
store i64 %a, i64* %RBP
store i64 %c, i64* %RCX
store i64 %RSP, i64* %RIP
store i64 %RSP+8, i64* %RSP
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

