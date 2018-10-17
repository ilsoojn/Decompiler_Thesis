define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
%RIP = alloca i64
store i64 %regset*, i64* %RIP
%EIP = alloca i32
store i32 %regset*, i32* %EIP
%RBP = alloca i64
store i64 %regset*, i64* %RBP
%RSP = alloca i64
store i64 %regset*, i64* %RSP
%ESP = alloca i32
store i32 %regset*, i32* %ESP
%EBP = alloca i32
store i32 %regset*, i32* %EBP
%RAX = alloca i64
store i64 %regset*, i64* %RAX
%EAX = alloca i32
store i32 %regset*, i32* %EAX
%ZMM0 = alloca <16 x float>
store <16 x float> %regset*, <16 x float>* %ZMM0
%XMM0 = alloca <4 x float>
store <4 x float> %regset*, <4 x float>* %XMM0
%YMM0 = alloca <8 x float>
store <8 x float> %regset*, <8 x float>* %YMM0
%ZMM1 = alloca <16 x float>
store <16 x float> %regset*, <16 x float>* %ZMM1
%XMM1 = alloca <4 x float>
store <4 x float> %regset*, <4 x float>* %XMM1
%YMM1 = alloca <8 x float>
store <8 x float> %regset*, <8 x float>* %YMM1
%RCX = alloca i64
store i64 %regset*, i64* %RCX
%ECX = alloca i32
store i32 %regset*, i32* %ECX
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
store i64 %RBP, i64* %RSP-8, align 1
%EAX_0 = %RAX
%EAX_1 = xor i32 %EAX_0, %EAX_0
%25 = load double, double* 4195672, align 1
%XMM0_0 = %ZMM0
%38 = load double, double* 4195680, align 1
%XMM1_0 = %ZMM1
%XMM1_1 = %XMM1_0 : %38
store i32 0, i32* %RSP-12, align 1
store i32 10, i32* %RSP-16, align 1
store double %38, double* %RSP-24, align 1
store double %25, double* %RSP-32, align 1
%64 = [ %RSP-16 ]
%77 = fmul double %25, %76
%90 = fadd double %25, %89
%XMM0_4 = %XMM0_0 : %25 : %64 : %77 : %90
store double %25, double* %RSP-40, align 1
store i32 %EAX_1, i32* %EAX
store i32 %RSP+8, i32* %EBP
store i32 [ %RSP-16 ], i32* %ECX
store i32 %RSP, i32* %EIP
store i32 %RSP+8, i32* %ESP
store i64 %EAX_1, i64* %RAX
store i64 [ %RSP+8 ], i64* %RBP
store i64 %RSP-16, i64* %RCX
store i64 %RSP, i64* %RIP
store i64 %RSP_3, i64* %RSP
store <4 x float> %25, <4 x float>* %XMM0
store <4 x float> %38, <4 x float>* %XMM1
store <8 x float> %XMM0_4, <8 x float>* %YMM0
store <8 x float> %XMM1_1, <8 x float>* %YMM1
store <16 x float> %XMM0_4, <16 x float>* %ZMM0
store <16 x float> %XMM1_1, <16 x float>* %ZMM1
br label %exit_fn_400480
}

