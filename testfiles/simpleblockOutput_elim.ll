define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
%RIP_init = %regset*
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %regset*
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_init = %regset*
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_init = %regset*
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = %regset*
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = %regset*
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%RAX_init = %regset*
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %regset*
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%ZMM0_init = %regset*
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%XMM0_init = %regset*
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%YMM0_init = %regset*
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_init = %regset*
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%XMM1_init = %regset*
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%YMM1_init = %regset*
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_init = %regset*
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %regset*
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
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
%RBP_0 = %RBP
%21 = %RSP-8
store i64 %RBP_0, i64* %21, align 1
%EAX_0 = %RAX
%EAX_1 = xor i32 %EAX_0, %EAX_0
%RAX_1 = %EAX_1
%24 = 4195470+202
%25 = load double, double* %24, align 1
%26 = %25
%XMM0_0 = %ZMM0
%XMM0_1 = %XMM0_0 : %26
%37 = 4195478+202
%38 = load double, double* %37, align 1
%39 = %38
%XMM1_0 = %ZMM1
%XMM1_1 = %XMM1_0 : %39
%50 = %RSP-8-4
store i32 0, i32* %50, align 1
%52 = %RSP-8-8
store i32 10, i32* %52, align 1
%56 = %RSP-8-16
store double %39, double* %56, align 1
%60 = %RSP-8-24
store double %26, double* %60, align 1
%ECX_0 = [ %RSP -16 ]
%RCX_1 = %RSP-16
%64 = [ %RSP -16 ]
%65 = %64
%XMM0_2 = %XMM0_1 : %65
%75 = %RSP-8-24
%76 = load double, double* %75, align 1
%77 = fmul double %65, %76
%78 = %77
%XMM0_3 = %XMM0_2 : %78
%88 = %RSP-8-16
%89 = load double, double* %88, align 1
%90 = fadd double %78, %89
%91 = %90
%XMM0_4 = %XMM0_3 : %91
%101 = %RSP-8-32
store double %91, double* %101, align 1
%RBP_1 = [ %RSP +8 ]
%EBP_1 = %RSP+8
%RIP_17 = %RSP
%ESP_2 = %RSP+8
%EIP_16 = %RSP
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
store <4 x float> %91, <4 x float>* %XMM0
store <4 x float> %39, <4 x float>* %XMM1
store <8 x float> %XMM0_4, <8 x float>* %YMM0
store <8 x float> %XMM1_1, <8 x float>* %YMM1
store <16 x float> %XMM0_4, <16 x float>* %ZMM0
store <16 x float> %XMM1_1, <16 x float>* %ZMM1
br label %exit_fn_400480
}

