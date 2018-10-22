define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
%RBP = alloca i64
%RAX = alloca i64
br label %bb_4004D0
exit_fn_4004D0:                                   ; preds = %bb_4005EB
ret void
bb_4004D0:                                        ; preds = %entry_fn_4004D0
store i64 %RBP, i64* %RSP-8, align 1
%26 = [4195992]
store i32 0, i32* %RSP-12, align 1
store i32 5, i32* %RSP-16, align 1
store double %26, double* %RSP-24, align 1
store i32 100, i32* %RSP-28, align 1
store i64 4195603, i64* %RSP-64
call void @fn_4003D0(%regset* %0)
store i32 0, i32* %RBP-24, align 1
store i32 %RAX, i32* %RBP-36, align 1
br label %bb_40051D
bb_40051D: ; preds = %bb_4005DD, %bb_4004D0
%119 = [%RBP-8]
br i1 ([%RBP-24]>=%119), label %bb_4005EB, label %bb_400529
bb_400529: ; preds = %bb_40051D
%145 = [%RBP-16]
%174 = fdiv double %145, [%RBP-8]
store double %174, double* %RBP-32, align 1
%188 = [%RBP-32]
%CC_BE_0 = or i1 ([%RBP-20]<%188), ([%RBP-20]==%188)
br i1 %CC_BE_0, label %bb_400589, label %bb_400554
bb_400554: ; preds = %bb_400529
%236 = [%RBP-32]
%251 = [%RBP-16]
%252 = fadd double %236, %251
store double %252, double* %RBP-16, align 1
store i64 4195713, i64* %RSP-8
call void @fn_4003D0(%regset* %0)
store i32 %RAX, i32* %RBP-40, align 1
br label %bb_4005D8
bb_400589: ; preds = %bb_400529
%325 = [%RBP-32]
%354 = fadd double [%RBP-20], %325
%XMM1_7 = %354
store i32 %XMM1_7, i32* %RBP-20, align 1
store double %393, double* %RBP-16, align 1
store i64 4195797, i64* %RSP-8
call void @fn_4003D0(%regset* %0)
store i32 %RAX, i32* %RBP-44, align 1
br label %bb_4005D8
bb_4005D8: ; preds = %bb_400589, %bb_400554
br label %bb_4005DD
bb_4005DD: ; preds = %bb_4005D8
store i32 %RBP-23, i32* %RBP-24, align 1
br label %bb_40051D
bb_4005EB: ; preds = %bb_40051D
store i64 4195836, i64* %RSP-8
call void @fn_4003D0(%regset* %0)
store i32 %RAX, i32* %RBP-48, align 1
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

