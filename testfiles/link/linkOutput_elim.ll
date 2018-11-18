define void @fn_4004F0(%regset* noalias nocapture) {
entry_fn_4004F0:
br label %bb_4004F0
exit_fn_4004F0:                                   ; preds = %bb_4004F0
ret void
bb_4004F0:                                        ; preds = %entry_fn_4004F0
%5 = call i8* @llvm.dc.translate.at(i8* 6295576)
call void %5(%regset* %0)
br label %exit_fn_4004F0
}

define void @fn_400650(%regset* noalias nocapture) {
%d = alloca i32, align 4
%c = alloca i32, align 4
%b = alloca i64, align 8
%a = alloca i64, align 8
entry_fn_400650:
%RBP = alloca i64
%RAX = alloca i64
%c = alloca i32
br label %bb_400650
exit_fn_400650: ; preds = %bb_400650
ret void
bb_400650: ; preds = %entry_fn_400650
store i64 %RBP, i64* %a, align 1
%12 = and i64 %RAX, -256
%RAX_1 = or i64 %12, 0
store i64 4195945, i64* %b
%CtlSysEFLAGS_0 = load i32, i32* %c
call void @fn_400500(%regset* %0)
%43 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %43, i32* %c
store i32 %RAX_1, i32* %d, align 1
store i32 %CtlSysEFLAGS_0, i32* %c
br label %exit_fn_400650
}

define void @fn_400500(%regset* noalias nocapture) {
entry_fn_400500:
br label %bb_400500
exit_fn_400500:                                   ; preds = %bb_400500
ret void
bb_400500:                                        ; preds = %entry_fn_400500
%5 = call i8* @llvm.dc.translate.at(i8* 6295584)
call void %5(%regset* %0)
br label %exit_fn_400500
}

define void @fn_400600(%regset* noalias nocapture) {
%i = alloca i32, align 4
%h = alloca i64, align 8
%g = alloca i64, align 8
%f = alloca i64, align 8
%e = alloca i32, align 4
%d = alloca i32, align 4
%c = alloca i64, align 8
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400600:
%RBP = alloca i64
%RAX = alloca i64
%d = alloca i32
br label %bb_400600
exit_fn_400600: ; preds = %bb_400600
ret void
bb_400600: ; preds = %entry_fn_400600
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
%17 = and i64 %RAX, -256
%RAX_1 = or i64 %17, 0
store i64 4195872, i64* %c
%CtlSysEFLAGS_0 = load i32, i32* %d
call void @fn_400500(%regset* %0)
%49 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %49, i32* %d
store i32 %RAX_1, i32* %e, align 1
%61 = and i64 %RAX_1, -256
%RAX_3 = or i64 %61, 0
store i64 4195882, i64* %f
store i32 %CtlSysEFLAGS_0, i32* %d
call void @fn_400650(%regset* %0)
%71 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %71, i32* %d
%81 = and i64 %RAX_3, -256
%RAX_5 = or i64 %81, 0
store i64 4195889, i64* %g
call void @fn_4004F0(%regset* %0)
%91 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %91, i32* %d
%101 = and i64 %RAX_5, -256
%RAX_7 = or i64 %101, 0
store i64 4195906, i64* %h
call void @fn_400500(%regset* %0)
%111 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %111, i32* %d
store i32 %RAX_7, i32* %i, align 1
%CtlSysEFLAGS_1 = load i32, i32* %d
store i32 %CtlSysEFLAGS_1, i32* %d
br label %exit_fn_400600
}

