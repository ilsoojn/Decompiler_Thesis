define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
%RBP = alloca i64
%ZMM0 = alloca <16 x float>
%ZMM1 = alloca <16 x float>
br label %bb_400480
exit_fn_400480: ; preds = %bb_400480
ret void
bb_400480: ; preds = %entry_fn_400480
store i64 %RBP, i64* %a, align 1
%25 = [4195672]
%26 = bitcast double %25 to i64
%XMM0_0 = %ZMM0
%XMM0_1 = %XMM0_0 : %26
%38 = [4195680]
%39 = bitcast double %38 to i64
%XMM1_0 = %ZMM1
store i32 0, i32* %b, align 1
store i32 10, i32* %c, align 1
%53 = %XMM1_0 : %39
%54 = bitcast i64 %53 to double
store double %54, double* %d, align 1
%57 = %XMM0_0 : %26
%58 = bitcast i64 %57 to double
store double %58, double* %e, align 1
%64 = [%RSP-16]
%65 = bitcast double %64 to i64
%XMM0_2 = %XMM0_1 : %65
%72 = %XMM0_1 : %65
%73 = bitcast i64 %72 to double
%76 = [%RSP-32]
%77 = fmul double %73, %76
%78 = bitcast double %77 to i64
%XMM0_3 = %XMM0_2 : %78
%85 = %XMM0_2 : %78
%86 = bitcast i64 %85 to double
%89 = [%RSP-24]
%90 = fadd double %86, %89
%91 = bitcast double %90 to i64
%98 = %XMM0_3 : %91
%99 = bitcast i64 %98 to double
store double %99, double* %f, align 1
br label %exit_fn_400480
}

