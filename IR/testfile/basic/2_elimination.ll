entry_fn_5FA:
%d = alloca i64, align 8
%c = alloca i64, align 8
%b = alloca i16, align 2
br label %bb_5FA
exit_fn_5FA: ; preds = %bb_5FA
ret void
bb_5FA: ; preds = %entry_fn_5FA
store i16 5, i16* %b, align 1
store i64 123, i64* %c, align 1
%RAX_0 = %c
%RDX_0 = ashr i64 %RAX_0, 63
%29 = shl i128 %RDX_0, 64
%30 = or i128 %29, %c
%32 = sdiv i128 %30, %b
store i64 %32, i64* %d, align 1
br label %exit_fn_5FA

