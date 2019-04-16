entry_fn_5FA:
%c = alloca i32, align 4
%b = alloca i16, align 2
br label %bb_5FA
exit_fn_5FA: ; preds = %bb_5FA
ret void
bb_5FA: ; preds = %entry_fn_5FA
store i16 -5, i16* %b, align 1
store i32 123, i32* %c, align 1
%EAX_0 = %c
%EDX_0 = ashr i32 %EAX_0, 31
%29 = shl i64 %EDX_0, 32
%30 = or i64 %29, %c
%32 = sdiv i64 %30, %b
store i32 %32, i32* %c, align 1
br label %exit_fn_5FA

