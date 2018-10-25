define void @fn_4003D0(%regset* noalias nocapture) {
entry_fn_4003D0:
br label %bb_4003D0
exit_fn_4003D0:                                   ; preds = %bb_4003D0
ret void
bb_4003D0:                                        ; preds = %entry_fn_4003D0
%5 = call i8* @llvm.dc.translate.at(i8* 6295576)
call void %5(%regset* %0)
br label %exit_fn_4003D0
}

define void @fn_4004D0(%regset* noalias nocapture) {
%ae = alloca i32, align 4
%ad = alloca i64, align 8
%ac = alloca i32, align 4
%ab = alloca i64, align 8
%k = alloca i32, align 4
%j = alloca i64, align 8
%i = alloca double, align 8
%h = alloca i32, align 4
%g = alloca i32, align 4
%f = alloca i64, align 8
%e = alloca i32, align 4
%d = alloca double, align 8
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_4004D0:
%RBP = alloca i64
%RAX = alloca i64
br label %bb_4004D0
exit_fn_4004D0: ; preds = %bb_4005EB
ret void
bb_4004D0: ; preds = %entry_fn_4004D0
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
store i32 5, i32* %c, align 1
store double 12345.4, double* %d, align 1
store i32 100, i32* %e, align 1
%61 = and i64 %RAX, -256
%RAX_1 = or i64 %61, 1
store i64 4195603, i64* %f
call void @fn_4003D0(%regset* %0)
store i32 0, i32* %g, align 1
store i32 %RAX_1, i32* %h, align 1
br label %bb_40051D
bb_40051D: ; preds = %bb_4005DD, %bb_4004D0
%119 = load i32, i32* %c, align 1
%CC_GE_0 = icmp sge i32 %g, %119
br i1 %CC_GE_0, label %bb_4005EB, label %bb_400529
bb_400529: ; preds = %bb_40051D
%145 = load double, double* %d, align 1
%174 = fdiv double %145, %c
store double %174, double* %i, align 1
%188 = load double, double* %i, align 1
%ZF_01 = fcmp ueq double %e, %188
%CF_03 = fcmp ult double %e, %188
%CC_BE_0 = or i1 %CF_03, %ZF_01
br i1 %CC_BE_0, label %bb_400589, label %bb_400554
bb_400554: ; preds = %bb_400529
%236 = load double, double* %i, align 1
%251 = load double, double* %d, align 1
%252 = fadd double %236, %251
store double %252, double* %d, align 1
%291 = and i64 %e, -256
%RAX_11 = or i64 %291, 2
store i64 4195713, i64* %j
call void @fn_4003D0(%regset* %0)
store i32 %RAX_11, i32* %k, align 1
br label %bb_4005D8
bb_400589: ; preds = %bb_400529
%325 = load double, double* %i, align 1
%354 = fadd double %e, %325
%XMM1_7 = %354
store i32 %XMM1_7, i32* %e, align 1
%381 = load double, double* %d, align 1
%393 = fsub double %381, %i
store double %393, double* %d, align 1
%429 = and i64 %i, -256
%RAX_17 = or i64 %429, 2
store i64 4195797, i64* %ab
call void @fn_4003D0(%regset* %0)
store i32 %RAX_17, i32* %ac, align 1
br label %bb_4005D8
bb_4005D8: ; preds = %bb_400589, %bb_400554
br label %bb_4005DD
bb_4005DD: ; preds = %bb_4005D8
store i32 %RSP-31, i32* %g, align 1
br label %bb_40051D
bb_4005EB: ; preds = %bb_40051D
%RAX_7 = or i64 %491, 0
store i64 4195836, i64* %ad
call void @fn_4003D0(%regset* %0)
store i32 %RAX_7, i32* %ae, align 1
br label %exit_fn_4004D0
}

