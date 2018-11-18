define void @fn_400480(%regset* noalias nocapture) {
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
%4 = bitcast <16 x float> %ZMM0_init to i512
%5 = trunc i512 %4 to i128
%XMM0_init = %ZMM0_ptr
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%6 = bitcast <16 x float> %ZMM0_init to i512
%7 = trunc i512 %6 to i256
%YMM0_init = %ZMM0_ptr
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%8 = bitcast <16 x float> %ZMM1_init to i512
%9 = trunc i512 %8 to i128
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%10 = bitcast <16 x float> %ZMM1_init to i512
%11 = trunc i512 %10 to i256
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
%R8 = alloca i64
store i64 %R8_init, i64* %R8
%R8D_init = %R8_ptr
%R8D = alloca i32
store i32 %R8D_init, i32* %R8D
%R8W_init = %R8_ptr
%R8W = alloca i16
store i16 %R8W_init, i16* %R8W
%R8B_init = %R8_ptr
%R8B = alloca i8
store i8 %R8B_init, i8* %R8B
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%CtlSysEFLAGS = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
br label %bb_400480
exit_fn_400480:                                   ; preds = %bb_400480
%12 = load i32, i32* %CtlSysEFLAGS
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
%22 = load i64, i64* %R8
store i64 %22, i64* %R8_ptr
%23 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %23, <16 x float>* %ZMM0_ptr
%24 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %24, <16 x float>* %ZMM1_ptr
ret void
bb_400480:                                        ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%26 = %RSP_0-8
store i64 %RBP_0, i64* %26, align 1
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
%27 = lshr i32 %EAX_1, 8
%EFLAGS_0 = %EFLAGS
%RIP_4 = 4195466
%EIP_3 = 4195466
%RCX_0 = %RSP-72
%ECX_0 = %RSP-72
%28 = lshr i64 %RCX_0, 8
%RIP_5 = 4195470
%EIP_4 = 4195470
%RDX_0 = %RSP-24
%EDX_0 = %RSP-24
%29 = lshr i64 %RDX_0, 8
%RIP_6 = 4195478
%EIP_5 = 4195478
%31 = %RIP_6+258
%32 = load double, double* %31, align 1
%33 = bitcast double %32 to i64
%ZMM0_0 = %ZMM0
%34 = bitcast <16 x float> %ZMM0_0 to i512
%XMM0_0 = %ZMM0
%XMM0_1 = %XMM0_0 : %33
%37 = bitcast <16 x float> %ZMM0_0 to i512
%YMM0_0 = %ZMM0
%YMM0_1 = %YMM0_0 : %XMM0_1
%40 = bitcast <16 x float> %ZMM0_0 to i512
%ZMM0_1 = %40 : %XMM0_1
%RIP_7 = 4195486
%EIP_6 = 4195486
%44 = %RIP_7+258
%45 = load float, float* %44, align 1
%46 = bitcast float %45 to i32
%ZMM1_0 = %ZMM1
%47 = bitcast <16 x float> %ZMM1_0 to i512
%XMM1_0 = %ZMM1
%XMM1_1 = %XMM1_0 : %46
%50 = bitcast <16 x float> %ZMM1_0 to i512
%YMM1_0 = %ZMM1
%YMM1_1 = %YMM1_0 : %XMM1_1
%53 = bitcast <16 x float> %ZMM1_0 to i512
%ZMM1_1 = %53 : %XMM1_1
%RIP_8 = 4195496
%EIP_7 = 4195496
%RIP_9 = 4195501
%EIP_8 = 4195501
%RDI_0 = %RDI
%RIP_10 = 4195504
%EIP_9 = 4195504
%R8_0 = %R8
%RIP_11 = 4195511
%EIP_10 = 4195511
%57 = %RSP_1-4
store i32 0, i32* %57, align 1
%RIP_12 = 4195517
%EIP_11 = 4195517
%59 = %RSP_1-6
store i16 -32100, i16* %59, align 1
%RIP_13 = 4195523
%EIP_12 = 4195523
%61 = %RSP_1-8
store i16 -104, i16* %61, align 1
%RIP_14 = 4195530
%EIP_13 = 4195530
%63 = %RSP_1-12
store i32 -4967296, i32* %63, align 1
%RIP_15 = 4195537
%EIP_14 = 4195537
%65 = %RSP_1-16
store i32 -2123123123, i32* %65, align 1
%RIP_16 = 4195545
%EIP_15 = 4195545
%67 = %RSP_1-24
store i64 -2111111111, i64* %67, align 1
%RIP_17 = 4195549
%EIP_16 = 4195549
%69 = %RSP_1-32
store i64 4200000000, i64* %69, align 1
%RIP_18 = 4195557
%EIP_17 = 4195557
%71 = %RSP_1-40
store i64 -123123123, i64* %71, align 1
%RIP_19 = 4195561
%EIP_18 = 4195561
%73 = %RSP_1-48
store i64 123123123123, i64* %73, align 1
%RIP_20 = 4195565
%EIP_19 = 4195565
%75 = %RSP_1-49
store i8 32, i8* %75, align 1
%RIP_21 = 4195569
%EIP_20 = 4195569
%77 = %RSP_1-50
store i8 33, i8* %77, align 1
%RIP_22 = 4195574
%EIP_21 = 4195574
%78 = trunc i128 %XMM1_1 to i32
%79 = bitcast i32 %78 to float
%81 = %RSP_1-56
store float %79, float* %81, align 1
%RIP_23 = 4195579
%EIP_22 = 4195579
%82 = trunc i128 %XMM0_1 to i64
%83 = bitcast i64 %82 to double
%85 = %RSP_1-64
store double %83, double* %85, align 1
%RIP_24 = 4195583
%EIP_23 = 4195583
%87 = %RSP_1-72
store i64 %RDX_0, i64* %87, align 1
%RIP_25 = 4195587
%EIP_24 = 4195587
%89 = %RSP_1-80
store i64 %RCX_0, i64* %89, align 1
%RIP_26 = 4195588
%EIP_25 = 4195588
%RSP_2 = %RSP
%ESP_1 = %RSP
%91 = %RSP_2-8
%RBP_1 = %RSP-8
%EBP_1 = %RSP-8
%RIP_27 = 4195589
%EIP_26 = 4195589
%RSP_3 = %RSP+8
%92 = inttoptr i64 %RSP_2 to i64*
%RIP_28 = %RSP
%ESP_2 = %RSP+8
%EIP_27 = %RSP
%ZF_0 = icmp eq i32 %EAX_1, 0
%SF_0 = icmp slt i32 %EAX_1, 0
%93 = trunc i32 %EAX_1 to i8
%94 = call i8 @llvm.ctpop.i8(i8 %93)
%95 = trunc i8 %94 to i1
%PF_0 = icmp eq i1 %95, false
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
%96 = zext i1 false to i32
%97 = shl i32 %96, 0
%98 = or i32 %97, %CtlSysEFLAGS_0
%99 = zext i1 %PF_0 to i32
%100 = shl i32 %99, 2
%101 = or i32 %100, %98
%102 = zext i1 false to i32
%103 = shl i32 %102, 4
%104 = or i32 %103, %101
%105 = zext i1 %ZF_0 to i32
%106 = shl i32 %105, 6
%107 = or i32 %106, %104
%108 = zext i1 %SF_0 to i32
%109 = shl i32 %108, 7
%110 = or i32 %109, %107
%111 = zext i1 false to i32
%112 = shl i32 %111, 11
%EFLAGS_1 = or i32 %110, %112
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %EAX_1, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %ECX_0, i32* %ECX
store i32 -94967296, i32* %EDI
store i32 %EDX_0, i32* %EDX
store i32 %EFLAGS_1, i32* %EFLAGS
store i32 %EIP_27, i32* %EIP
store i32 -1430928461, i32* %ESI
store i32 %ESP_2, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 %RCX_0, i64* %RCX
store i64 4200000000, i64* %RDI
store i64 %RDX_0, i64* %RDX
store i64 %RIP_28, i64* %RIP
store i64 123123123123, i64* %RSI
store i64 %RSP_3, i64* %RSP
store i64 4200000000, i64* %R8
%113 = bitcast i128 %XMM0_1 to <4 x float>
store <4 x float> %113, <4 x float>* %XMM0
%114 = bitcast i128 %XMM1_1 to <4 x float>
store <4 x float> %114, <4 x float>* %XMM1
%115 = bitcast i256 %YMM0_1 to <8 x float>
store <8 x float> %115, <8 x float>* %YMM0
%116 = bitcast i256 %YMM1_1 to <8 x float>
store <8 x float> %116, <8 x float>* %YMM1
%117 = bitcast i512 %ZMM0_1 to <16 x float>
store <16 x float> %117, <16 x float>* %ZMM0
%118 = bitcast i512 %ZMM1_1 to <16 x float>
store <16 x float> %118, <16 x float>* %ZMM1
store i8 0, i8* %R8B
store i32 -94967296, i32* %R8D
store i16 -5632, i16* %R8W
br label %exit_fn_400480
}

