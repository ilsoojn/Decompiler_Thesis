define void @fn_400480(%regset* noalias nocapture) {
%ak = alloca i16, align 2
%aj = alloca i32, align 4
%ai = alloca i8, align 1
%ah = alloca i64, align 8
%ag = alloca i32, align 4
%af = alloca i64, align 8
%ae = alloca i64, align 8
%ad = alloca double, align 8
%ac = alloca float, align 4
%ab = alloca i8, align 1
%k = alloca i8, align 1
%j = alloca i64, align 8
%i = alloca i64, align 8
%h = alloca i64, align 8
%g = alloca i64, align 8
%f = alloca i32, align 4
%e = alloca i32, align 4
%d = alloca i16, align 2
%c = alloca i16, align 2
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
%EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
%EFLAGS_init = %EFLAGS_ptr
%EFLAGS = alloca i32
store i32 %EFLAGS_init, i32* %EFLAGS
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %RCX_ptr
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%2 = lshr i64 %RCX_init, 8
%RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
%RDX_init = %RDX_ptr
%RDX = alloca i64
store i64 %RDX_init, i64* %RDX
%EDX_init = %RDX_ptr
%EDX = alloca i32
store i32 %EDX_init, i32* %EDX
%3 = lshr i64 %RDX_init, 8
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%4 = %ZMM0_init
%5 = %ZMM0_init
%XMM0_init = %ZMM0_ptr
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%6 = %ZMM0_init
%7 = %ZMM0_init
%YMM0_init = %ZMM0_ptr
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%8 = %ZMM1_init
%9 = %ZMM1_init
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%10 = %ZMM1_init
%11 = %ZMM1_init
%YMM1_init = %ZMM1_ptr
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
%RSI_init = %RSI_ptr
%RSI = alloca i64
store i64 %RSI_init, i64* %RSI
%ESI_init = %RSI_ptr
%ESI = alloca i32
store i32 %ESI_init, i32* %ESI
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = %RDI_ptr
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = %RDI_ptr
%EDI = alloca i32
store i32 %EDI_init, i32* %EDI
%R8_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 69
%R8_init = %R8_ptr
%ah = alloca i64
store i64 %R8_init, i64* %ah
%R8D_init = %R8_ptr
%aj = alloca i32
store i32 %R8D_init, i32* %aj
%R8W_init = %R8_ptr
%ak = alloca i16
store i16 %R8W_init, i16* %ak
%R8B_init = %R8_ptr
%ai = alloca i8
store i8 %R8B_init, i8* %ai
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%ag = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %ag
br label %bb_400480
exit_fn_400480: ; preds = %bb_400480
%12 = load i32, i32* %ag
store i32 %12, i32* %CtlSysEFLAGS_ptr
%13 = load i32, i32* %EFLAGS
store i32 %13, i32* %EFLAGS_ptr
%14 = load i64, i64* %RAX
store i64 %14, i64* %RAX_ptr
%15 = load i64, i64* %RBP
store i64 %15, i64* %RBP_ptr
%16 = load i64, i64* %RCX
store i64 %16, i64* %RCX_ptr
%17 = load i64, i64* %RDI
store i64 %17, i64* %RDI_ptr
%18 = load i64, i64* %RDX
store i64 %18, i64* %RDX_ptr
%19 = load i64, i64* %RIP
store i64 %19, i64* %RIP_ptr
%20 = load i64, i64* %RSI
store i64 %20, i64* %RSI_ptr
%21 = load i64, i64* %RSP
store i64 %21, i64* %RSP_ptr
%22 = load i64, i64* %ah
store i64 %22, i64* %R8_ptr
%23 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %23, <16 x float>* %ZMM0_ptr
%24 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %24, <16 x float>* %ZMM1_ptr
ret void
bb_400480: ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%26 = %a
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
%27 = lshr i32 %EAX_1, 8
%EFLAGS_0 = %EFLAGS
%RIP_4 = 4195466
%EIP_3 = 4195466
%RCX_0 = %ad
%ECX_0 = %ad
%28 = lshr i64 %ad, 8
%RIP_5 = 4195470
%EIP_4 = 4195470
%RDX_0 = %f
%EDX_0 = %f
%29 = lshr i64 %f, 8
%RIP_6 = 4195478
%EIP_5 = 4195478
%31 = 4195736
%32 = load double, double* 4195736, align 1
%33 = %32
%ZMM0_0 = %ZMM0
%34 = %ZMM0
%XMM0_0 = %ZMM0
%XMM0_1 = %32
%37 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %32
%40 = %ZMM0
%ZMM0_1 = %32
%RIP_7 = 4195486
%EIP_6 = 4195486
%44 = 4195744
%45 = load float, float* 4195744, align 1
%46 = %45
%ZMM1_0 = %ZMM1
%47 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = %45
%50 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = %45
%53 = %ZMM1
%ZMM1_1 = %45
%RIP_8 = 4195496
%EIP_7 = 4195496
%RIP_9 = 4195501
%EIP_8 = 4195501
%RDI_0 = %RDI
%RIP_10 = 4195504
%EIP_9 = 4195504
%R8_0 = %ah
%RIP_11 = 4195511
%EIP_10 = 4195511
%57 = %b
store i32 0, i32* %b, align 1
%RIP_12 = 4195517
%EIP_11 = 4195517
%59 = %c
store i16 -32100, i16* %c, align 1
%RIP_13 = 4195523
%EIP_12 = 4195523
%61 = %d
store i16 -104, i16* %d, align 1
%RIP_14 = 4195530
%EIP_13 = 4195530
%63 = %e
store i32 -4967296, i32* %e, align 1
%RIP_15 = 4195537
%EIP_14 = 4195537
%65 = %f
store i32 -2123123123, i32* %f, align 1
%RIP_16 = 4195545
%EIP_15 = 4195545
%67 = %g
store i64 -2111111111, i64* %g, align 1
%RIP_17 = 4195549
%EIP_16 = 4195549
%69 = %h
store i64 4200000000, i64* %h, align 1
%RIP_18 = 4195557
%EIP_17 = 4195557
%71 = %i
store i64 -123123123, i64* %i, align 1
%RIP_19 = 4195561
%EIP_18 = 4195561
%73 = %j
store i64 123123123123, i64* %j, align 1
%RIP_20 = 4195565
%EIP_19 = 4195565
%75 = %k
store i8 32, i8* %k, align 1
%RIP_21 = 4195569
%EIP_20 = 4195569
%77 = %ab
store i8 33, i8* %ab, align 1
%RIP_22 = 4195574
%EIP_21 = 4195574
%78 = %45
%79 = %45
%81 = %ac
store float %45, float* %ac, align 1
%RIP_23 = 4195579
%EIP_22 = 4195579
%82 = %32
%83 = %32
%85 = %ad
store double %32, double* %ad, align 1
%RIP_24 = 4195583
%EIP_23 = 4195583
%87 = %ae
store i64 %f, i64* %ae, align 1
%RIP_25 = 4195587
%EIP_24 = 4195587
%89 = %af
store i64 %ad, i64* %af, align 1
%RIP_26 = 4195588
%EIP_25 = 4195588
%RSP_2 = %RSP
%ESP_1 = %RSP
%91 = %a
%RBP_1 = %a
%EBP_1 = %a
%RIP_27 = 4195589
%EIP_26 = 4195589
%RSP_3 = %RSP+8
%92 = %RSP
%RIP_28 = %RSP
%ESP_2 = %RSP+8
%EIP_27 = %RSP
%ZF_0 = icmp eq i32 %EAX_1, 0
%SF_0 = icmp slt i32 %EAX_1, 0
%93 = %EAX_1
%94 = call i8 @llvm.ctpop.i8(i8 %EAX_1)
%95 = %94
%PF_0 = icmp eq i1 %94, false
%CtlSysEFLAGS_0 = load i32, i32* %ag
%96 = false
%97 = shl i32 false, 0
%98 = or i32 %97, %CtlSysEFLAGS_0
%99 = %PF_0
%100 = shl i32 %PF_0, 2
%101 = or i32 %100, %98
%102 = false
%103 = shl i32 false, 4
%104 = or i32 %103, %101
%105 = %ZF_0
%106 = shl i32 %ZF_0, 6
%107 = or i32 %106, %104
%108 = %SF_0
%109 = shl i32 %SF_0, 7
%110 = or i32 %109, %107
%111 = false
%112 = shl i32 false, 11
%EFLAGS_1 = or i32 %110, %112
store i32 %CtlSysEFLAGS_0, i32* %ag
store i32 %EAX_1, i32* %EAX
store i32 %a, i32* %EBP
store i32 %ad, i32* %ECX
store i32 -94967296, i32* %EDI
store i32 %f, i32* %EDX
store i32 %EFLAGS_1, i32* %EFLAGS
store i32 %RSP, i32* %EIP
store i32 -1430928461, i32* %ESI
store i32 %RSP+8, i32* %ESP
store i64 %EAX_1, i64* %RAX
store i64 %a, i64* %RBP
store i64 %ad, i64* %RCX
store i64 4200000000, i64* %RDI
store i64 %f, i64* %RDX
store i64 %RSP, i64* %RIP
store i64 123123123123, i64* %RSI
store i64 %RSP+8, i64* %RSP
store i64 4200000000, i64* %ah
%113 = %32
store <4 x float> %32, <4 x float>* %XMM0
%114 = %45
store <4 x float> %45, <4 x float>* %XMM1
%115 = %32
store <8 x float> %32, <8 x float>* %YMM0
%116 = %45
store <8 x float> %45, <8 x float>* %YMM1
%117 = %32
store <16 x float> %32, <16 x float>* %ZMM0
%118 = %45
store <16 x float> %45, <16 x float>* %ZMM1
store i8 0, i8* %ai
store i32 -94967296, i32* %aj
store i16 -5632, i16* %ak
br label %exit_fn_400480
}

