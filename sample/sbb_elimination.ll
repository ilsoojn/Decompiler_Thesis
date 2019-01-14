define void @fn_400480(%regset* noalias nocapture) {
%f = alloca double, align 8
%e = alloca double, align 8
%d = alloca double, align 8
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RIP = alloca i64
%EIP = alloca i32
%RBP = alloca i64
%RSP = alloca i64
%EBP = alloca i32
%RAX = alloca i64
%EAX = alloca i32
%ZMM0 = alloca <16 x float>
%XMM0 = alloca <4 x float>
%YMM0 = alloca <8 x float>
%ZMM1 = alloca <16 x float>
%XMM1 = alloca <4 x float>
%YMM1 = alloca <8 x float>
%RCX = alloca i64
%ECX = alloca i32
%CtlSysEFLAGS = alloca i32
br label %bb_400480
exit_fn_400480: ; preds = %bb_400480
ret void
bb_400480: ; preds = %entry_fn_400480
store i64 %RBP, i64* %a, align 1
%EAX_0 = %RAX
%EAX_1 = xor i32 %EAX_0, %EAX_0
store i32 0, i32* %b, align 1
store i32 10, i32* %c, align 1
store double 313.24, double* %d, align 1
store double 10.0, double* %e, align 1
%76 = load double, double* %e, align 1
%77 = fmul double %c, %76
%89 = load double, double* %d, align 1
%90 = fadd double %77, %89
store double %90, double* %f, align 1
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %EAX_1, i32* %EAX
store i32 %a, i32* %EBP
store i32 %c, i32* %ECX
store i32 %RSP, i32* %EIP
store i64 %EAX_1, i64* %RAX
store i64 %a, i64* %RBP
store i64 %c, i64* %RCX
store i64 %RSP, i64* %RIP
store <4 x float> %90, <4 x float>* %XMM0
store <4 x float> 313.24, <4 x float>* %XMM1
store <8 x float> %90, <8 x float>* %YMM0
store <8 x float> 313.24, <8 x float>* %YMM1
store <16 x float> %90, <16 x float>* %ZMM0
store <16 x float> 313.24, <16 x float>* %ZMM1
br label %exit_fn_400480
}

