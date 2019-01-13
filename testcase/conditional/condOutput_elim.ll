define void @fn_400480(%regset* noalias nocapture) {
%i = alloca i32, align 4
%f = alloca i32, align 4
%e = alloca i32, align 4
%d = alloca double, align 8
%c = alloca double, align 8
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
%CtlSysEFLAGS = alloca i32
%RAX = alloca i64
%EAX = alloca i32
br label %bb_400480
exit_fn_400480: ; preds = %bb_40053F
ret void
bb_400480: ; preds = %entry_fn_400480
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
store double -43.2, double* %c, align 1
store double -32.1, double* %d, align 1
%60 = load double, double* %c, align 1
%72 = load double, double* %d, align 1
%ZF_0 = fcmp ueq double %60, %72
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
%CC_NE_0 = xor i1 %ZF_0, true
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %a, i32* %EBP
store i32 4195527, i32* %EIP
store i32 %a, i32* %ESP
store i64 %a, i64* %RBP
store i64 4195527, i64* %RIP
store i64 %a, i64* %RSP
store <4 x float> %60, <4 x float>* %XMM0
store <4 x float> -43.2, <4 x float>* %XMM1
store <8 x float> %60, <8 x float>* %YMM0
store <8 x float> -43.2, <8 x float>* %YMM1
store <16 x float> %60, <16 x float>* %ZMM0
store <16 x float> -43.2, <16 x float>* %ZMM1
br i1 %CC_NE_0, label %bb_4004C7, label %bb_4004B5
bb_4004B5: ; preds = %bb_400480
store i32 4195527, i32* %EIP
store i64 4195527, i64* %RIP
bb_4004BB: ; preds = %bb_4004B5
store i32 0, i32* %e, align 1
store i32 4195571, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195571, i64* %RIP
br label %bb_4004F3
bb_4004C7: ; preds = %bb_4004B5, %bb_400480
%101 = load double, double* %c, align 1
%114 = load double, double* %d, align 1
%ZF_02 = fcmp ueq double %114, %101
%CF_04 = fcmp ult double %114, %101
%CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
%CC_BE_0 = or i1 %CF_04, %ZF_02
store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
store i32 4195559, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195559, i64* %RIP
store <4 x float> %101, <4 x float>* %XMM0
store <4 x float> %114, <4 x float>* %XMM1
store <8 x float> %101, <8 x float>* %YMM0
store <8 x float> %114, <8 x float>* %YMM1
store <16 x float> %101, <16 x float>* %ZMM0
store <16 x float> %114, <16 x float>* %ZMM1
br i1 %CC_BE_0, label %bb_4004E7, label %bb_4004DB
bb_4004DB: ; preds = %bb_4004C7
store i32 -1, i32* %e, align 1
store i32 4195566, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195566, i64* %RIP
br label %bb_4004EE
bb_4004E7: ; preds = %bb_4004C7
store i32 1, i32* %e, align 1
store i32 4195566, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195566, i64* %RIP
br label %bb_4004EE
bb_4004EE: ; preds = %bb_4004E7, %bb_4004DB
store i32 4195571, i32* %EIP
store i64 4195571, i64* %RIP
br label %bb_4004F3
bb_4004F3: ; preds = %bb_4004EE, %bb_4004BB
store i32 %e, i32* %f, align 1
%CtlSysEFLAGS_2 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
store i32 %e, i32* %EAX
store i32 4195616, i32* %EIP
store i64 %e, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195616, i64* %RIP
bb_400507: ; preds = %bb_4004F3
store i32 4195596, i32* %EIP
store i64 4195596, i64* %RIP
br label %bb_40050C
bb_40050C: ; preds = %bb_400507
%CtlSysEFLAGS_3 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
store i32 4195628, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195628, i64* %RIP
bb_40051B: ; preds = %bb_40050C
store i32 4195640, i32* %EIP
store i64 4195640, i64* %RIP
br label %bb_400538
bb_400520: ; preds = %bb_4004F3
store i32 -1, i32* %i, align 1
store i32 4195647, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_40052C: ; preds = %bb_40050C
store i32 1, i32* %i, align 1
store i32 4195647, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_400538: ; preds = %bb_40051B
store i32 0, i32* %i, align 1
store i32 4195647, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_40053F: ; preds = %bb_400538, %bb_40052C, %bb_400520
store i32 %i, i32* %EAX
store i32 %a, i32* %EBP
store i32 %RSP, i32* %EIP
store i64 %i, i64* %RAX
store i64 %a, i64* %RBP
store i64 %RSP, i64* %RIP
br label %exit_fn_400480
}

