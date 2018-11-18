define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
br label %bb_4004D0
exit_fn_4004D0:                                   ; preds = %bb_4004D0
ret void
bb_4004D0:                                        ; preds = %entry_fn_4004D0
%5 = call i8* @llvm.dc.translate.at(i8* 6295600)
call void %5(%regset* %0)
br label %exit_fn_4004D0
}

define void @fn_4004B0(%regset* noalias nocapture) {
entry_fn_4004B0:
br label %bb_4004B0
exit_fn_4004B0:                                   ; preds = %bb_4004B0
ret void
bb_4004B0:                                        ; preds = %entry_fn_4004B0
%5 = call i8* @llvm.dc.translate.at(i8* 6295584)
call void %5(%regset* %0)
br label %exit_fn_4004B0
}

define void @fn_4004C0(%regset* noalias nocapture) {
entry_fn_4004C0:
br label %bb_4004C0
exit_fn_4004C0:                                   ; preds = %bb_4004C0
ret void
bb_4004C0:                                        ; preds = %entry_fn_4004C0
%5 = call i8* @llvm.dc.translate.at(i8* 6295592)
call void %5(%regset* %0)
br label %exit_fn_4004C0
}

define void @fn_4004A0(%regset* noalias nocapture) {
entry_fn_4004A0:
br label %bb_4004A0
exit_fn_4004A0:                                   ; preds = %bb_4004A0
ret void
bb_4004A0:                                        ; preds = %entry_fn_4004A0
%5 = call i8* @llvm.dc.translate.at(i8* 6295576)
call void %5(%regset* %0)
br label %exit_fn_4004A0
}

define void @fn_4005D0(%regset* noalias nocapture) {
%k = alloca i32, align 4
%j = alloca i64, align 8
%i = alloca i32, align 4
%h = alloca i64, align 8
%g = alloca i32, align 4
%f = alloca i64, align 8
%e = alloca i32, align 4
%d = alloca i32, align 4
%c = alloca i64, align 8
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_4005D0:
%RBP = alloca i64
%d = alloca i32
%RAX = alloca i64
br label %bb_4005D0
exit_fn_4005D0: ; preds = %bb_4005D0
ret void
bb_4005D0: ; preds = %entry_fn_4005D0
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
store i64 4195822, i64* %c
%CtlSysEFLAGS_0 = load i32, i32* %d
call void @fn_4004A0(%regset* %0)
%48 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %48, i32* %d
store i32 %RAX, i32* %e, align 1
%61 = and i64 %RAX, -256
%RAX_1 = or i64 %61, 0
store i64 4195836, i64* %f
store i32 %CtlSysEFLAGS_0, i32* %d
call void @fn_4004C0(%regset* %0)
%72 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %72, i32* %d
store i32 %RAX_1, i32* %g, align 1
%85 = and i64 %RAX_1, -256
%RAX_3 = or i64 %85, 0
store i64 4195856, i64* %h
call void @fn_4004B0(%regset* %0)
%96 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %96, i32* %d
store i32 %RAX_3, i32* %i, align 1
%109 = and i64 %RAX_3, -256
%RAX_5 = or i64 %109, 0
store i64 4195880, i64* %j
call void @fn_4004D0(%regset* %0)
%120 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %120, i32* %d
store i32 %RAX_5, i32* %k, align 1
%CtlSysEFLAGS_1 = load i32, i32* %d
store i32 %CtlSysEFLAGS_1, i32* %d
br label %exit_fn_4005D0
}

