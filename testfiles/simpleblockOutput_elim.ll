define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
%RBP = alloca i64
br label %bb_400480
exit_fn_400480:                                   ; preds = %bb_400480
ret void
bb_400480:                                        ; preds = %entry_fn_400480
store i64 %RBP, i64* %RSP-8, align 1
%25 = 10.0
%38 = 313.24
store i32 0, i32* %RSP-12, align 1
store i32 10, i32* %RSP-16, align 1
store double 313.24, double* %RSP-24, align 1
store double 10.0, double* %RSP-32, align 1
%76 = [%RSP-32]
%77 = fmul double [%RSP-16], %76
%89 = [%RSP-24]
%90 = fadd double %77, %89
store double %90, double* %RSP-40, align 1
br label %exit_fn_400480
}

