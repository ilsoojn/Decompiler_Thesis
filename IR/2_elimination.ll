entry_fn_5FA:
%d = alloca i32, align 4
%c = alloca double, align 8
%b = alloca double, align 8
br label %bb_5FA
exit_fn_5FA: ; preds = %bb_647
ret void
bb_5FA: ; preds = %entry_fn_5FA
store double 12.3, double* %b, align 1
store double 12.3, double* %c, align 1
store i32 0, i32* %d, align 1
%50 = load double, double* %b, align 1
%62 = load double, double* %c, align 1
%PF_0 = fcmp uno double %50, %62
br i1 %PF_0, label %bb_640, label %bb_62B
bb_62B: ; preds = %bb_5FA
%85 = load double, double* %b, align 1
%100 = load double, double* %c, align 1
%ZF_01 = fcmp ueq double %85, %100
%CC_NE_0 = xor i1 %ZF_01, true
br i1 %CC_NE_0, label %bb_640, label %bb_637
bb_637: ; preds = %bb_62B
store i32 1, i32* %d, align 1
br label %bb_647
bb_640: ; preds = %bb_62B, %bb_5FA
store i32 -1, i32* %d, align 1
br label %bb_647
bb_647: ; preds = %bb_637, %bb_640
br label %exit_fn_5FA

