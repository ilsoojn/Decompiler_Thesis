define void @fn_4004C0(%regset* noalias nocapture) {
entry_fn_4004C0:
br label %bb_4004C0
exit_fn_4004C0:                                   ; preds = %bb_4004C0
ret void
bb_4004C0:                                        ; preds = %entry_fn_4004C0
br label %exit_fn_4004C0
}

define void @fn_4004A0(%regset* noalias nocapture) {
entry_fn_4004A0:
br label %bb_4004A0
exit_fn_4004A0:                                   ; preds = %bb_4004A0
ret void
bb_4004A0:                                        ; preds = %entry_fn_4004A0
br label %exit_fn_4004A0
}

define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
br label %bb_4004D0
exit_fn_4004D0:                                   ; preds = %bb_4004D0
ret void
bb_4004D0:                                        ; preds = %entry_fn_4004D0
br label %exit_fn_4004D0
}

define void @fn_4004B0(%regset* noalias nocapture) {
entry_fn_4004B0:
br label %bb_4004B0
exit_fn_4004B0:                                   ; preds = %bb_4004B0
ret void
bb_4004B0:                                        ; preds = %entry_fn_4004B0
br label %exit_fn_4004B0
}

define void @fn_4005D0(%regset* noalias nocapture) {
%j = alloca i32, align 4
%i = alloca i64, align 8
%h = alloca i32, align 4
%g = alloca i64, align 8
%f = alloca i32, align 4
%e = alloca i64, align 8
%d = alloca i32, align 4
%c = alloca i64, align 8
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_4005D0:
%RBP = alloca i64
%RAX = alloca i64
br label %bb_4005D0
exit_fn_4005D0: ; preds = %bb_4005D0
ret void
bb_4005D0: ; preds = %entry_fn_4005D0
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
%18 = and i64 %RAX, -256
%RAX_1 = or i64 %18, 0
store i64 4195824, i64* %c
call void @fn_4004B0(%regset* %0)
store i32 %RAX_1, i32* %d, align 1
%64 = and i64 %RAX_1, -256
%RAX_3 = or i64 %64, 0
store i64 4195848, i64* %e
call void @fn_4004D0(%regset* %0)
store i32 %RAX_3, i32* %f, align 1
store i64 4195866, i64* %g
call void @fn_4004A0(%regset* %0)
store i32 %RAX_3, i32* %h, align 1
%109 = and i64 %RAX_3, -256
%RAX_6 = or i64 %109, 0
store i64 4195880, i64* %i
call void @fn_4004C0(%regset* %0)
store i32 %RAX_6, i32* %j, align 1
br label %exit_fn_4005D0
}

