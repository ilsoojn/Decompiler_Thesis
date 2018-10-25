define void @fn_4004D0(%regset* noalias nocapture) {
%ad = alloca i32, align 4
%ac = alloca i32, align 4
%ab = alloca i32, align 4
%k = alloca i32, align 4
%j = alloca double, align 8
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
%26 = 12345.4
store i32 0, i32* %b, align 1
store i32 5, i32* %c, align 1
store double 12345.4, double* %d, align 1
store i32 100, i32* %e, align 1
store i64 4195603, i64* %f
call void @fn_4003D0(%regset* %0)
store i32 0, i32* %g, align 1
store i32 %RAX, i32* %h, align 1
br label %bb_40051D
bb_40051D: ; preds = %bb_4005DD, %bb_4004D0
%119 = [%RBP-8]
br i1 ([%RBP-24]>=%119), label %bb_4005EB, label %bb_400529
bb_400529: ; preds = %bb_40051D
%145 = [%j]
%174 = fdiv double %145, [%RBP-8]
store double %174, double* %i, align 1
%188 = [%i]
%CC_BE_0 = or i1 ([%RBP-20]<%188), ([%RBP-20]==%188)
br i1 %CC_BE_0, label %bb_400589, label %bb_400554
bb_400554: ; preds = %bb_400529
%236 = [%i]
%251 = [%j]
%252 = fadd double %236, %251
store double %252, double* %j, align 1
store i64 4195713, i64* %a
call void @fn_4003D0(%regset* %0)
store i32 %RAX, i32* %k, align 1
br label %bb_4005D8
bb_400589: ; preds = %bb_400529
%325 = [%i]
%354 = fadd double [%ab], %325
%XMM1_7 = %354
store i32 %XMM1_7, i32* %ab, align 1
store double %393, double* %j, align 1
store i64 4195797, i64* %a
call void @fn_4003D0(%regset* %0)
store i32 %RAX, i32* %ac, align 1
br label %bb_4005D8
bb_4005D8: ; preds = %bb_400589, %bb_400554
br label %bb_4005DD
bb_4005DD: ; preds = %bb_4005D8
store i32 %RBP-23, i32* %g, align 1
br label %bb_40051D
bb_4005EB: ; preds = %bb_40051D
store i64 4195836, i64* %a
call void @fn_4003D0(%regset* %0)
store i32 %RAX, i32* %ad, align 1
br label %exit_fn_4004D0
}

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

