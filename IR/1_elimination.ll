entry_fn_5FA:
%d = alloca i64, align 8
%c = alloca i32, align 4
%b = alloca i16, align 2
br label %bb_5FA
exit_fn_5FA: ; preds = %bb_5FA
ret void
bb_5FA: ; preds = %entry_fn_5FA
store i16 5, i16* %b, align 1
store i32 123, i32* %c, align 1
store i64 43210, i64* %d, align 1
%27 = load i64, i64* %d, align 1
store i64 %27 EAX_1, i64* %d, align 1
br label %exit_fn_5FA

