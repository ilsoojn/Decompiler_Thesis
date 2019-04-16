entry_fn_5FA:
%d = alloca double, align 8
%c = alloca double, align 8
%b = alloca float, align 4
br label %bb_5FA
exit_fn_5FA: ; preds = %bb_5FA
ret void
bb_5FA: ; preds = %entry_fn_5FA
store float 11.109999656677246, float* %b, align 1
store double 12345.678, double* %c, align 1
%51 = load float, float* %b, align 1
%62 = load double, double* %c, align 1
%77 = fdiv double %62, %51
%XMM1_2 = %77
%92 = load float, float* %b, align 1
%105 = fsub double %XMM1_2, %92
store double %105, double* %d, align 1
br label %exit_fn_5FA

