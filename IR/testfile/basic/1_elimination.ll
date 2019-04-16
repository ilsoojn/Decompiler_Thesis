entry_fn_5FA:
%d = alloca i64, align 8
%c = alloca i32, align 4
%b = alloca i16, align 2
%a = alloca i64, align 8
%RIP = alloca i64
%EIP = alloca i32
%RBP = alloca i64
%RSP = alloca i64
%EBP = alloca i32
%RAX = alloca i64
%EAX = alloca i32
%CtlSysEFLAGS = alloca i32
br label %bb_5FA
exit_fn_5FA: ; preds = %bb_5FA
ret void
bb_5FA: ; preds = %entry_fn_5FA
store i64 %RBP, i64* %a, align 1
store i16 5, i16* %b, align 1
store i32 123, i32* %c, align 1
store i64 43210, i64* %d, align 1
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 0, i32* %EAX
store i32 %a, i32* %EBP
store i32 %RSP, i32* %EIP
store i64 0, i64* %RAX
store i64 %a, i64* %RBP
store i64 %RSP, i64* %RIP
br label %exit_fn_5FA

