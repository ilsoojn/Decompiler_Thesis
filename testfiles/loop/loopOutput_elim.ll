define void @fn_400480(%regset* noalias nocapture) {
%g = alloca double, align 8
%f = alloca double, align 8
%e = alloca double, align 8
%d = alloca i32, align 4
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RBP = alloca i64
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
br label %bb_4004B3
bb_4004B3: ; preds = %bb_4004BF, %bb_400480
%71 = load i32, i32* %d, align 1
%CC_GE_0 = icmp sge i32 %c, %71
br i1 %CC_GE_0, label %bb_4004CB, label %bb_4004BF
bb_4004BF: ; preds = %bb_4004B3
store i32 %RSP-11, i32* %c, align 1
br label %bb_4004B3
bb_4004CB: ; preds = %bb_4004B3
br label %bb_4004D0
bb_4004D0: ; preds = %bb_4004E0, %bb_4004CB
%123 = load double, double* %e, align 1
%138 = load double, double* %f, align 1
%CF_06 = fcmp ult double %123, %138
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
br label %bb_4004D0
bb_400503: ; preds = %bb_4004D0
br label %exit_fn_400480
}

