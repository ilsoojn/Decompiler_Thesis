entry_fn_5FA:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %RIP_ptr
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
%RBP_init = %RBP_ptr
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
%RSP_init = %RSP_ptr
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = %RSP_ptr
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = %RBP_ptr
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%1 = lshr i64 %RAX_init, 8
%EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
%EFLAGS_init = %EFLAGS_ptr
%EFLAGS = alloca i32
store i32 %EFLAGS_init, i32* %EFLAGS
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%CtlSysEFLAGS = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
br label %bb_5FA
exit_fn_5FA:                                      ; preds = %bb_5FA
%2 = load i32, i32* %CtlSysEFLAGS
store i32 %2, i32* %CtlSysEFLAGS_ptr
%3 = load i32, i32* %EFLAGS
store i32 %3, i32* %EFLAGS_ptr
%4 = load i64, i64* %RAX
store i64 %4, i64* %RAX_ptr
%5 = load i64, i64* %RBP
store i64 %5, i64* %RBP_ptr
%6 = load i64, i64* %RIP
store i64 %6, i64* %RIP_ptr
%7 = load i64, i64* %RSP
store i64 %7, i64* %RSP_ptr
ret void
bb_5FA:                                           ; preds = %entry_fn_5FA
%RIP_1 = 1531
%EIP_0 = 1531
%RBP_0 = %RBP
%RSP_0 = %RSP
%9 = %RSP-8
store i64 %RBP, i64* %RSP-8, align 1
%RSP_1 = %RSP+8
%ESP_0 = %RSP-8
%RIP_2 = 1534
%EIP_1 = 1534
%EBP_0 = %RSP-8
%RIP_3 = 1540
%EIP_2 = 1540
%11 = %RSP-22
store i16 5, i16* %RSP-22, align 1
%RIP_4 = 1547
%EIP_3 = 1547
%13 = %RSP-20
store i32 123, i32* %RSP-20, align 1
%RIP_5 = 1555
%EIP_4 = 1555
%15 = %RSP-16
store i64 43210, i64* %RSP-16, align 1
%RIP_6 = 1559
%EIP_5 = 1559
%17 = %RSP-22
%18 = load i16, i16* %RSP-22, align 1
%EAX_0 = %RSP-22
%RAX_0 = %RAX
%RAX_1 = %RSP-22
%19 = lshr i32 %EAX_0, 8
%RIP_7 = 1563
%EIP_6 = 1563
%21 = %RSP-20
%22 = load i32, i32* %RSP-20, align 1
%EAX_1 = %22 * %EAX_0
%RAX_2 = %EAX_1
%23 = lshr i32 %EAX_1, 8
%EFLAGS_0 = %EFLAGS
%RIP_8 = 1565
%EIP_7 = 1565
%RAX_3 = %EAX_1
%24 = lshr i32 %EAX_1, 8
%RIP_9 = 1569
%EIP_8 = 1569
%26 = %RSP-16
%27 = load i64, i64* %RSP-16, align 1
%28 = %27 - %EAX_1
store i64 %27 EAX_1, i64* %RSP-16, align 1
%RIP_10 = 1574
%EIP_9 = 1574
%RIP_11 = 1575
%EIP_10 = 1575
%RSP_2 = %RSP+0
%ESP_1 = %RSP
%30 = %RSP-8
%RBP_1 = %RSP-8
%EBP_1 = %RSP-8
%RIP_12 = 1576
%EIP_11 = 1576
%RSP_3 = %RSP+8
%31 = %RSP+0
%RIP_13 = %RSP
%ESP_2 = %RSP+8
%EIP_12 = %RSP
%ZF_0 = icmp eq i64 %27 EAX_1, 0
%SF_0 = icmp slt i64 %27 EAX_1, 0
%32 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %27, i64 %RAX_3)
%OF_0 = extractvalue { i64, i1 } %32, 1
%33 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %27, i64 %RAX_3)
%CF_0 = extractvalue { i64, i1 } %33, 1
%34 = EAX_1
%35 = call i8 @llvm.ctpop.i8(i8 EAX_1)
%36 = %35
%PF_0 = icmp eq i1 %35, false
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
%37 = %33
%38 = shl i32 %33, 0
%39 = or i32 %38, %CtlSysEFLAGS_0
%40 = %PF_0
%41 = shl i32 %PF_0, 2
%42 = or i32 %41, %39
%43 = false
%44 = shl i32 false, 4
%45 = or i32 %44, %42
%46 = %ZF_0
%47 = shl i32 %ZF_0, 6
%48 = or i32 %47, %45
%49 = %SF_0
%50 = shl i32 %SF_0, 7
%51 = or i32 %50, %48
%52 = %32
%53 = shl i32 %32, 11
%EFLAGS_1 = or i32 %51, %53
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 0, i32* %EAX
store i32 %RSP-8, i32* %EBP
store i32 %EFLAGS_1, i32* %EFLAGS
store i32 %RSP, i32* %EIP
store i32 %RSP+8, i32* %ESP
store i64 0, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 %RSP, i64* %RIP
store i64 %RSP+8, i64* %RSP
br label %exit_fn_5FA

