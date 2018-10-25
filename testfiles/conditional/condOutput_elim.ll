define void @fn_400480(%regset* noalias nocapture) {
%i = alloca i32, align 4
%h = alloca i32, align 4
%g = alloca i32, align 4
%f = alloca i32, align 4
%e = alloca i32, align 4
%d = alloca double, align 8
%c = alloca double, align 8
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RBP = alloca i64
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
%CC_NE_0 = xor i1 %ZF_0, true
br i1 %CC_NE_0, label %bb_4004C7, label %bb_4004B5
bb_4004B5: ; preds = %bb_400480
bb_4004BB: ; preds = %bb_4004B5
store i32 0, i32* %e, align 1
br label %bb_4004F3
bb_4004C7: ; preds = %bb_4004B5, %bb_400480
%101 = load double, double* %c, align 1
%114 = load double, double* %d, align 1
%ZF_02 = fcmp ueq double %114, %101
%CF_04 = fcmp ult double %114, %101
%CC_BE_0 = or i1 %CF_04, %ZF_02
br i1 %CC_BE_0, label %bb_4004E7, label %bb_4004DB
bb_4004DB: ; preds = %bb_4004C7
store i32 -1, i32* %e, align 1
br label %bb_4004EE
bb_4004E7: ; preds = %bb_4004C7
store i32 1, i32* %e, align 1
br label %bb_4004EE
bb_4004EE: ; preds = %bb_4004E7, %bb_4004DB
br label %bb_4004F3
bb_4004F3: ; preds = %bb_4004EE, %bb_4004BB
store i32 %e, i32* %f, align 1
store i32 %RSP-35, i32* %g, align 1
bb_400507: ; preds = %bb_4004F3
br label %bb_40050C
bb_40050C: ; preds = %bb_400507
store i32 %RSP-45, i32* %h, align 1
bb_40051B: ; preds = %bb_40050C
br label %bb_400538
bb_400520: ; preds = %bb_4004F3
store i32 -1, i32* %i, align 1
br label %bb_40053F
bb_40052C: ; preds = %bb_40050C
store i32 1, i32* %i, align 1
br label %bb_40053F
bb_400538: ; preds = %bb_40051B
store i32 0, i32* %i, align 1
br label %bb_40053F
bb_40053F: ; preds = %bb_400538, %bb_40052C, %bb_400520
br label %exit_fn_400480
}

