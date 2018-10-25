define void @fn_400480(%regset* noalias nocapture) {
%f = alloca double, align 8
%e = alloca double, align 8
%d = alloca double, align 8
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RBP = alloca i64
br label %bb_400480
exit_fn_400480: ; preds = %bb_400480
ret void
bb_400480: ; preds = %entry_fn_400480
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
store i32 10, i32* %c, align 1
store double 313.24, double* %d, align 1
store double 10.0, double* %e, align 1
%76 = load double, double* %e, align 1
%77 = fmul double %c, %76
%89 = load double, double* %d, align 1
%90 = fadd double %77, %89
store double %90, double* %f, align 1
br label %exit_fn_400480
}

