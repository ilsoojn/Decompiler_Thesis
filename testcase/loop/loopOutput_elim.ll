define void @fn_400480(%regset* noalias nocapture) {
%g = alloca double, align 8
%f = alloca double, align 8
%e = alloca double, align 8
%d = alloca i32, align 4
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RIP = alloca i64
%EIP = alloca i32
%RBP = alloca i64
%RSP = alloca i64
%ESP = alloca i32
%EBP = alloca i32
%ZMM0 = alloca <16 x float>
%XMM0 = alloca <4 x float>
%YMM0 = alloca <8 x float>
%ZMM1 = alloca <16 x float>
%XMM1 = alloca <4 x float>
%YMM1 = alloca <8 x float>
%RAX = alloca i64
%EAX = alloca i32
%CtlSysEFLAGS = alloca i32
br label %bb_400480
exit_fn_400480: ; preds = %bb_400503
ret void
bb_400480: ; preds = %entry_fn_400480
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
store i32 5, i32* %c, align 1
store i32 625, i32* %d, align 1
store double 5.5, double* %e, align 1
store double 1.2, double* %f, align 1
store i32 %a, i32* %EBP
store i32 4195507, i32* %EIP
store i32 %a, i32* %ESP
store i64 %a, i64* %RBP
store i64 4195507, i64* %RIP
store i64 %a, i64* %RSP
store <4 x float> 1.2, <4 x float>* %XMM0
store <4 x float> 5.5, <4 x float>* %XMM1
store <8 x float> 1.2, <8 x float>* %YMM0
store <8 x float> 5.5, <8 x float>* %YMM1
store <16 x float> 1.2, <16 x float>* %ZMM0
store <16 x float> 5.5, <16 x float>* %ZMM1
br label %bb_4004B3
bb_4004B3: ; preds = %bb_4004BF, %bb_400480
%71 = load i32, i32* %d, align 1
%CC_GE_0 = icmp sge i32 %c, %71
%CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
store i32 %c, i32* %EAX
store i32 4195531, i32* %EIP
store i64 %c, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195531, i64* %RIP
br i1 %CC_GE_0, label %bb_4004CB, label %bb_4004BF
bb_4004BF: ; preds = %bb_4004B3
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 4195507, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195507, i64* %RIP
br label %bb_4004B3
bb_4004CB: ; preds = %bb_4004B3
store i32 4195536, i32* %EIP
store i64 4195536, i64* %RIP
br label %bb_4004D0
bb_4004D0: ; preds = %bb_4004E0, %bb_4004CB
%123 = load double, double* %e, align 1
%138 = load double, double* %f, align 1
%CF_06 = fcmp ult double %123, %138
%CtlSysEFLAGS_2 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
store i32 4195587, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195587, i64* %RIP
store <4 x float> %123, <4 x float>* %XMM0
store <8 x float> %123, <8 x float>* %YMM0
store <16 x float> %123, <16 x float>* %ZMM0
br i1 %CF_06, label %bb_400503, label %bb_4004E0
bb_4004E0: ; preds = %bb_4004D0
%161 = load double, double* %e, align 1
%176 = load double, double* %f, align 1
%177 = fdiv double %161, %176
store double %177, double* %g, align 1
%191 = load double, double* %g, align 1
%203 = load double, double* %f, align 1
%204 = fadd double %191, %203
store double %204, double* %f, align 1
store i32 4195536, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195536, i64* %RIP
store <4 x float> %204, <4 x float>* %XMM0
store <8 x float> %204, <8 x float>* %YMM0
store <16 x float> %204, <16 x float>* %ZMM0
br label %bb_4004D0
bb_400503: ; preds = %bb_4004D0
%CtlSysEFLAGS_3 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
store i32 %a, i32* %EBP
store i32 %RSP, i32* %EIP
store i64 %a, i64* %RBP
store i64 %RSP, i64* %RIP
br label %exit_fn_400480
}

