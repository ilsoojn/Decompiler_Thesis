define void @fn_4004F0(%regset* noalias nocapture) {
entry_fn_4004F0:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %RIP_ptr
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
br label %bb_4004F0
exit_fn_4004F0:                                   ; preds = %bb_4004F0
%1 = load i64, i64* %RIP
store i64 %1, i64* %RIP_ptr
ret void
bb_4004F0:                                        ; preds = %entry_fn_4004F0
%RIP_1 = 4195574
%EIP_0 = 4195574
%3 = 6295576
%RIP_2 = 6295576
%EIP_1 = 6295576
%4 = 6295576
%5 = call i8* @llvm.dc.translate.at(i8* 6295576)
%6 = %5
store i32 6295576, i32* %EIP
store i64 6295576, i64* %RIP
%7 = load i64, i64* %RIP
store i64 %7, i64* %RIP_ptr
call void %5(%regset* %0)
%8 = load i64, i64* %RIP_ptr
store i64 %8, i64* %RIP
br label %exit_fn_4004F0
}

define void @fn_400650(%regset* noalias nocapture) {
entry_fn_400650:
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
%EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
%EFLAGS_init = %EFLAGS_ptr
%EFLAGS = alloca i32
store i32 %EFLAGS_init, i32* %EFLAGS
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = %RDI_ptr
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = %RDI_ptr
%EDI = alloca i32
store i32 %EDI_init, i32* %EDI
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%CtlSysEFLAGS = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
br label %bb_400650
exit_fn_400650:                                   ; preds = %bb_400650
%1 = load i32, i32* %CtlSysEFLAGS
store i32 %1, i32* %CtlSysEFLAGS_ptr
%2 = load i32, i32* %EFLAGS
store i32 %2, i32* %EFLAGS_ptr
%3 = load i64, i64* %RAX
store i64 %3, i64* %RAX_ptr
%4 = load i64, i64* %RBP
store i64 %4, i64* %RBP_ptr
%5 = load i64, i64* %RDI
store i64 %5, i64* %RDI_ptr
%6 = load i64, i64* %RIP
store i64 %6, i64* %RIP_ptr
%7 = load i64, i64* %RSP
store i64 %7, i64* %RSP_ptr
ret void
bb_400650:                                        ; preds = %entry_fn_400650
%RIP_1 = 4195921
%EIP_0 = 4195921
%RBP_0 = %RBP
%RSP_0 = %RSP
%9 = %RSP-8
store i64 %RBP, i64* %RSP-8, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195924
%EIP_1 = 4195924
%EBP_0 = %RSP-8
%RIP_3 = 4195928
%EIP_2 = 4195928
%RSP_2 = %RSP-24
%ESP_1 = %RSP-24
%EFLAGS_0 = %EFLAGS
%RIP_4 = 4195938
%EIP_3 = 4195938
%RIP_5 = 4195940
%EIP_4 = 4195940
%RAX_0 = %RAX
%EAX_0 = %RAX
%11 = and i32 %RAX, -256
%EAX_1 = or i32 %11, 0
%12 = and i64 %RAX, -256
%RAX_1 = or i64 %12, 0
%RIP_6 = 4195945
%EIP_5 = 4195945
%RSP_3 = %RSP-32
%13 = %RSP-32
store i64 4195945, i64* %RSP-32
%ESP_2 = %RSP-32
store i32 %EAX_1, i32* %EAX
store i32 %RSP-8, i32* %EBP
store i32 4196122, i32* %EDI
%ZF_0 = icmp eq i64 %RSP-24, 0
%SF_0 = icmp slt i64 %RSP-24, 0
%14 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP-8, i64 16)
%OF_0 = extractvalue { i64, i1 } %14, 1
%16 = %RSP-24
%17 = call i8 @llvm.ctpop.i8(i8 %RSP-24)
%18 = %17
%PF_0 = icmp eq i1 %17, false
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
%22 = %PF_0
%23 = shl i32 %PF_0, 2
%25 = false
%26 = shl i32 false, 4
%28 = %ZF_0
%29 = shl i32 %ZF_0, 6
%31 = %SF_0
%32 = shl i32 %SF_0, 7
%34 = %14
%35 = shl i32 %14, 11
store i32 4195945, i32* %EIP
store i32 %RSP-32, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 4196122, i64* %RDI
store i64 4195945, i64* %RIP
store i64 %RSP-32, i64* %RSP
%36 = load i32, i32* %CtlSysEFLAGS
store i32 %36, i32* %CtlSysEFLAGS_ptr
%37 = load i32, i32* %EFLAGS
store i32 %37, i32* %EFLAGS_ptr
%38 = load i64, i64* %RAX
store i64 %38, i64* %RAX_ptr
%39 = load i64, i64* %RBP
store i64 %39, i64* %RBP_ptr
%40 = load i64, i64* %RDI
store i64 %40, i64* %RDI_ptr
%41 = load i64, i64* %RIP
store i64 %41, i64* %RIP_ptr
%42 = load i64, i64* %RSP
store i64 %42, i64* %RSP_ptr
call void @fn_400500(%regset* %0)
%43 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %43, i32* %CtlSysEFLAGS
%44 = load i32, i32* %EFLAGS_ptr
store i32 %44, i32* %EFLAGS
%45 = load i64, i64* %RAX_ptr
store i64 %45, i64* %RAX
%46 = load i64, i64* %RBP_ptr
store i64 %46, i64* %RBP
%47 = load i64, i64* %RDI_ptr
store i64 %47, i64* %RDI
%48 = load i64, i64* %RIP_ptr
store i64 %48, i64* %RIP
%49 = load i64, i64* %RSP_ptr
store i64 %49, i64* %RSP
%RIP_7 = 4195945
%RIP_8 = 4195948
%EIP_6 = 4195948
%RAX_2 = %RAX_1
%EAX_2 = %RAX_1
%RBP_1 = %RSP-8
%51 = %RSP-12
store i32 %RAX_1, i32* %RSP-12, align 1
%RIP_9 = 4195952
%EIP_7 = 4195952
%RSP_4 = %RSP-32
%RSP_5 = %RSP-16
%ESP_3 = %RSP-16
%EFLAGS_2 = %EFLAGS
%RIP_10 = 4195953
%EIP_8 = 4195953
%RSP_6 = %RSP-8
%ESP_4 = %RSP-8
%53 = %RSP-16
%RBP_2 = %RSP-16
%EBP_1 = %RSP-16
%RIP_11 = 4195954
%EIP_9 = 4195954
%RSP_7 = %RSP
%54 = %RSP-8
%RIP_12 = %RSP-8
%ESP_5 = %RSP
%EIP_10 = %RSP-8
%ZF_1 = icmp eq i64 %RSP-16, 0
%SF_1 = icmp slt i64 %RSP-16, 0
%55 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP-32, i64 16)
%OF_1 = extractvalue { i64, i1 } %55, 1
%56 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP-32, i64 16)
%CF_1 = extractvalue { i64, i1 } %56, 1
%57 = %RSP-16
%58 = call i8 @llvm.ctpop.i8(i8 %RSP-16)
%60 = %56
%61 = shl i32 %56, 0
%62 = or i32 %61, %CtlSysEFLAGS_0
%66 = false
%67 = shl i32 false, 4
%69 = %ZF_1
%70 = shl i32 %ZF_1, 6
%72 = %SF_1
%73 = shl i32 %SF_1, 7
%75 = %55
%76 = shl i32 %55, 11
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %RAX_1, i32* %EAX
store i32 %RSP-16, i32* %EBP
store i32 %RSP-8, i32* %EIP
store i32 %RSP, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RSP-16, i64* %RBP
store i64 %RSP-8, i64* %RIP
store i64 %RSP, i64* %RSP
br label %exit_fn_400650
}

define void @fn_400500(%regset* noalias nocapture) {
entry_fn_400500:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %RIP_ptr
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
br label %bb_400500
exit_fn_400500:                                   ; preds = %bb_400500
%1 = load i64, i64* %RIP
store i64 %1, i64* %RIP_ptr
ret void
bb_400500:                                        ; preds = %entry_fn_400500
%RIP_1 = 4195590
%EIP_0 = 4195590
%3 = 6295584
%RIP_2 = 6295584
%EIP_1 = 6295584
%4 = 6295584
%5 = call i8* @llvm.dc.translate.at(i8* 6295584)
%6 = %5
store i32 6295584, i32* %EIP
store i64 6295584, i64* %RIP
%7 = load i64, i64* %RIP
store i64 %7, i64* %RIP_ptr
call void %5(%regset* %0)
%8 = load i64, i64* %RIP_ptr
store i64 %8, i64* %RIP
br label %exit_fn_400500
}

define void @fn_400600(%regset* noalias nocapture) {
entry_fn_400600:
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
%EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
%EFLAGS_init = %EFLAGS_ptr
%EFLAGS = alloca i32
store i32 %EFLAGS_init, i32* %EFLAGS
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = %RDI_ptr
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = %RDI_ptr
%EDI = alloca i32
store i32 %EDI_init, i32* %EDI
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%CtlSysEFLAGS = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %RCX_ptr
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%1 = lshr i64 %RCX_init, 8
%2 = lshr i64 %RAX_init, 8
br label %bb_400600
exit_fn_400600:                                   ; preds = %bb_400600
%3 = load i32, i32* %CtlSysEFLAGS
store i32 %3, i32* %CtlSysEFLAGS_ptr
%4 = load i32, i32* %EFLAGS
store i32 %4, i32* %EFLAGS_ptr
%5 = load i64, i64* %RAX
store i64 %5, i64* %RAX_ptr
%6 = load i64, i64* %RBP
store i64 %6, i64* %RBP_ptr
%7 = load i64, i64* %RCX
store i64 %7, i64* %RCX_ptr
%8 = load i64, i64* %RDI
store i64 %8, i64* %RDI_ptr
%9 = load i64, i64* %RIP
store i64 %9, i64* %RIP_ptr
%10 = load i64, i64* %RSP
store i64 %10, i64* %RSP_ptr
ret void
bb_400600:                                        ; preds = %entry_fn_400600
%RIP_1 = 4195841
%EIP_0 = 4195841
%RBP_0 = %RBP
%RSP_0 = %RSP
%12 = %RSP-8
store i64 %RBP, i64* %RSP-8, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195844
%EIP_1 = 4195844
%EBP_0 = %RSP-8
%RIP_3 = 4195848
%EIP_2 = 4195848
%RSP_2 = %RSP-24
%ESP_1 = %RSP-24
%EFLAGS_0 = %EFLAGS
%RIP_4 = 4195858
%EIP_3 = 4195858
%RIP_5 = 4195865
%EIP_4 = 4195865
%14 = %RSP-12
store i32 0, i32* %RSP-12, align 1
%RIP_6 = 4195867
%EIP_5 = 4195867
%RAX_0 = %RAX
%EAX_0 = %RAX
%16 = and i32 %RAX, -256
%EAX_1 = or i32 %16, 0
%17 = and i64 %RAX, -256
%RAX_1 = or i64 %17, 0
%RIP_7 = 4195872
%EIP_6 = 4195872
%RSP_3 = %RSP-32
%18 = %RSP-32
store i64 4195872, i64* %RSP-32
%ESP_2 = %RSP-32
store i32 %EAX_1, i32* %EAX
store i32 %RSP-8, i32* %EBP
store i32 4196100, i32* %EDI
%ZF_0 = icmp eq i64 %RSP-24, 0
%SF_0 = icmp slt i64 %RSP-24, 0
%19 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP-8, i64 16)
%OF_0 = extractvalue { i64, i1 } %19, 1
%20 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP-8, i64 16)
%CF_0 = extractvalue { i64, i1 } %20, 1
%21 = %RSP-24
%22 = call i8 @llvm.ctpop.i8(i8 %RSP-24)
%23 = %22
%PF_0 = icmp eq i1 %22, false
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
%24 = %20
%25 = shl i32 %20, 0
%26 = or i32 %25, %CtlSysEFLAGS_0
%27 = %PF_0
%28 = shl i32 %PF_0, 2
%29 = or i32 %28, %26
%30 = false
%31 = shl i32 false, 4
%32 = or i32 %31, %29
%33 = %ZF_0
%34 = shl i32 %ZF_0, 6
%35 = or i32 %34, %32
%36 = %SF_0
%37 = shl i32 %SF_0, 7
%38 = or i32 %37, %35
%39 = %19
%40 = shl i32 %19, 11
%EFLAGS_1 = or i32 %38, %40
store i32 %EFLAGS_1, i32* %EFLAGS
store i32 4195872, i32* %EIP
store i32 %RSP-32, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 4196100, i64* %RDI
store i64 4195872, i64* %RIP
store i64 %RSP-32, i64* %RSP
%41 = load i32, i32* %CtlSysEFLAGS
store i32 %41, i32* %CtlSysEFLAGS_ptr
%42 = load i32, i32* %EFLAGS
store i32 %42, i32* %EFLAGS_ptr
%43 = load i64, i64* %RAX
store i64 %43, i64* %RAX_ptr
%44 = load i64, i64* %RBP
store i64 %44, i64* %RBP_ptr
%45 = load i64, i64* %RCX
store i64 %45, i64* %RCX_ptr
%46 = load i64, i64* %RDI
store i64 %46, i64* %RDI_ptr
%47 = load i64, i64* %RIP
store i64 %47, i64* %RIP_ptr
%48 = load i64, i64* %RSP
store i64 %48, i64* %RSP_ptr
call void @fn_400500(%regset* %0)
%49 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %49, i32* %CtlSysEFLAGS
%50 = load i32, i32* %EFLAGS_ptr
store i32 %50, i32* %EFLAGS
%51 = load i64, i64* %RAX_ptr
store i64 %51, i64* %RAX
%52 = load i64, i64* %RBP_ptr
store i64 %52, i64* %RBP
%53 = load i64, i64* %RCX_ptr
store i64 %53, i64* %RCX
%54 = load i64, i64* %RDI_ptr
store i64 %54, i64* %RDI
%55 = load i64, i64* %RIP_ptr
store i64 %55, i64* %RIP
%56 = load i64, i64* %RSP_ptr
store i64 %56, i64* %RSP
%RIP_8 = 4195872
%RIP_9 = 4195875
%EIP_7 = 4195875
%RAX_2 = %RAX_1
%EAX_2 = %RAX_1
%RBP_1 = %RSP-8
%58 = %RSP-16
store i32 %RAX_1, i32* %RSP-16, align 1
%RIP_10 = 4195877
%EIP_8 = 4195877
%60 = and i32 %RAX_1, -256
%EAX_3 = or i32 %60, 0
%61 = and i64 %RAX_1, -256
%RAX_3 = or i64 %61, 0
%RIP_11 = 4195882
%EIP_9 = 4195882
%RSP_4 = %RSP-32
%RSP_5 = %RSP-40
%62 = %RSP-40
store i64 4195882, i64* %RSP-40
%ESP_3 = %RSP-40
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %EAX_3, i32* %EAX
store i32 4195882, i32* %EIP
store i32 %RSP-40, i32* %ESP
store i64 %RAX_3, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 4195882, i64* %RIP
store i64 %RSP-40, i64* %RSP
%63 = load i32, i32* %CtlSysEFLAGS
store i32 %63, i32* %CtlSysEFLAGS_ptr
%64 = load i32, i32* %EFLAGS
store i32 %64, i32* %EFLAGS_ptr
%65 = load i64, i64* %RAX
store i64 %65, i64* %RAX_ptr
%66 = load i64, i64* %RBP
store i64 %66, i64* %RBP_ptr
%67 = load i64, i64* %RCX
store i64 %67, i64* %RCX_ptr
%68 = load i64, i64* %RDI
store i64 %68, i64* %RDI_ptr
%69 = load i64, i64* %RIP
store i64 %69, i64* %RIP_ptr
%70 = load i64, i64* %RSP
store i64 %70, i64* %RSP_ptr
call void @fn_400650(%regset* %0)
%71 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %71, i32* %CtlSysEFLAGS
%72 = load i32, i32* %EFLAGS_ptr
store i32 %72, i32* %EFLAGS
%73 = load i64, i64* %RAX_ptr
store i64 %73, i64* %RAX
%74 = load i64, i64* %RBP_ptr
store i64 %74, i64* %RBP
%75 = load i64, i64* %RCX_ptr
store i64 %75, i64* %RCX
%76 = load i64, i64* %RDI_ptr
store i64 %76, i64* %RDI
%77 = load i64, i64* %RIP_ptr
store i64 %77, i64* %RIP
%78 = load i64, i64* %RSP_ptr
store i64 %78, i64* %RSP
%RIP_12 = 4195882
%RIP_13 = 4195884
%EIP_10 = 4195884
%RAX_4 = %RAX_3
%EAX_4 = %RAX_3
%80 = and i32 %RAX_3, -256
%EAX_5 = or i32 %80, 0
%81 = and i64 %RAX_3, -256
%RAX_5 = or i64 %81, 0
%RIP_14 = 4195889
%EIP_11 = 4195889
%RSP_6 = %RSP-40
%RSP_7 = %RSP-48
%82 = %RSP-48
store i64 4195889, i64* %RSP-48
%ESP_4 = %RSP-48
store i32 %EAX_5, i32* %EAX
store i32 4195889, i32* %EIP
store i32 %RSP-48, i32* %ESP
store i64 %RAX_5, i64* %RAX
store i64 4195889, i64* %RIP
store i64 %RSP-48, i64* %RSP
%83 = load i32, i32* %CtlSysEFLAGS
store i32 %83, i32* %CtlSysEFLAGS_ptr
%84 = load i32, i32* %EFLAGS
store i32 %84, i32* %EFLAGS_ptr
%85 = load i64, i64* %RAX
store i64 %85, i64* %RAX_ptr
%86 = load i64, i64* %RBP
store i64 %86, i64* %RBP_ptr
%87 = load i64, i64* %RCX
store i64 %87, i64* %RCX_ptr
%88 = load i64, i64* %RDI
store i64 %88, i64* %RDI_ptr
%89 = load i64, i64* %RIP
store i64 %89, i64* %RIP_ptr
%90 = load i64, i64* %RSP
store i64 %90, i64* %RSP_ptr
call void @fn_4004F0(%regset* %0)
%91 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %91, i32* %CtlSysEFLAGS
%92 = load i32, i32* %EFLAGS_ptr
store i32 %92, i32* %EFLAGS
%93 = load i64, i64* %RAX_ptr
store i64 %93, i64* %RAX
%94 = load i64, i64* %RBP_ptr
store i64 %94, i64* %RBP
%95 = load i64, i64* %RCX_ptr
store i64 %95, i64* %RCX
%96 = load i64, i64* %RDI_ptr
store i64 %96, i64* %RDI
%97 = load i64, i64* %RIP_ptr
store i64 %97, i64* %RIP
%98 = load i64, i64* %RSP_ptr
store i64 %98, i64* %RSP
%RIP_15 = 4195889
%RIP_16 = 4195899
%EIP_12 = 4195899
%RIP_17 = 4195901
%EIP_13 = 4195901
%RAX_6 = %RAX_5
%EAX_6 = %RAX_5
%100 = and i32 %RAX_5, -256
%EAX_7 = or i32 %100, 0
%101 = and i64 %RAX_5, -256
%RAX_7 = or i64 %101, 0
%RIP_18 = 4195906
%EIP_14 = 4195906
%RSP_8 = %RSP-48
%RSP_9 = %RSP-56
%102 = %RSP-56
store i64 4195906, i64* %RSP-56
%ESP_5 = %RSP-56
store i32 %EAX_7, i32* %EAX
store i32 4196112, i32* %EDI
store i32 4195906, i32* %EIP
store i32 %RSP-56, i32* %ESP
store i64 %RAX_7, i64* %RAX
store i64 4196112, i64* %RDI
store i64 4195906, i64* %RIP
store i64 %RSP-56, i64* %RSP
%103 = load i32, i32* %CtlSysEFLAGS
store i32 %103, i32* %CtlSysEFLAGS_ptr
%104 = load i32, i32* %EFLAGS
store i32 %104, i32* %EFLAGS_ptr
%105 = load i64, i64* %RAX
store i64 %105, i64* %RAX_ptr
%106 = load i64, i64* %RBP
store i64 %106, i64* %RBP_ptr
%107 = load i64, i64* %RCX
store i64 %107, i64* %RCX_ptr
%108 = load i64, i64* %RDI
store i64 %108, i64* %RDI_ptr
%109 = load i64, i64* %RIP
store i64 %109, i64* %RIP_ptr
%110 = load i64, i64* %RSP
store i64 %110, i64* %RSP_ptr
call void @fn_400500(%regset* %0)
%111 = load i32, i32* %CtlSysEFLAGS_ptr
store i32 %111, i32* %CtlSysEFLAGS
%112 = load i32, i32* %EFLAGS_ptr
store i32 %112, i32* %EFLAGS
%113 = load i64, i64* %RAX_ptr
store i64 %113, i64* %RAX
%114 = load i64, i64* %RBP_ptr
store i64 %114, i64* %RBP
%115 = load i64, i64* %RCX_ptr
store i64 %115, i64* %RCX
%116 = load i64, i64* %RDI_ptr
store i64 %116, i64* %RDI
%117 = load i64, i64* %RIP_ptr
store i64 %117, i64* %RIP
%118 = load i64, i64* %RSP_ptr
store i64 %118, i64* %RSP
%RIP_19 = 4195906
%RIP_20 = 4195908
%EIP_15 = 4195908
%RCX_0 = %RCX
%ECX_0 = %RCX
%ECX_1 = xor i32 %ECX_0, %ECX_0
%RCX_1 = %ECX_1
%119 = lshr i32 %ECX_1, 8
%EFLAGS_2 = %EFLAGS_1
%RIP_21 = 4195911
%EIP_16 = 4195911
%RAX_8 = %RAX_7
%EAX_8 = %RAX_7
%RBP_2 = %RSP-8
%121 = %RSP-20
store i32 %RAX_7, i32* %RSP-20, align 1
%RIP_22 = 4195913
%EIP_17 = 4195913
%RAX_9 = %ECX_1
%122 = lshr i32 %ECX_1, 8
%RIP_23 = 4195917
%EIP_18 = 4195917
%RSP_10 = %RSP-56
%RSP_11 = %RSP-40
%ESP_6 = %RSP-40
%RIP_24 = 4195918
%EIP_19 = 4195918
%RSP_12 = %RSP-32
%ESP_7 = %RSP-32
%124 = %RSP-40
%RBP_3 = %RSP-40
%EBP_1 = %RSP-40
%RIP_25 = 4195919
%EIP_20 = 4195919
%RSP_13 = %RSP-24
%125 = %RSP-32
%RIP_26 = %RSP-32
%ESP_8 = %RSP-24
%EIP_21 = %RSP-32
%ZF_1 = icmp eq i64 %RSP-40, 0
%SF_1 = icmp slt i64 %RSP-40, 0
%126 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP-56, i64 16)
%OF_1 = extractvalue { i64, i1 } %126, 1
%127 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP-56, i64 16)
%CF_1 = extractvalue { i64, i1 } %127, 1
%128 = %RSP-40
%129 = call i8 @llvm.ctpop.i8(i8 %RSP-40)
%130 = %129
%PF_1 = icmp eq i1 %129, false
%CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
%131 = %127
%132 = shl i32 %127, 0
%133 = or i32 %132, %CtlSysEFLAGS_1
%134 = %PF_1
%135 = shl i32 %PF_1, 2
%136 = or i32 %135, %133
%137 = false
%138 = shl i32 false, 4
%139 = or i32 %138, %136
%140 = %ZF_1
%141 = shl i32 %ZF_1, 6
%142 = or i32 %141, %139
%143 = %SF_1
%144 = shl i32 %SF_1, 7
%145 = or i32 %144, %142
%146 = %126
%147 = shl i32 %126, 11
%EFLAGS_3 = or i32 %145, %147
store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
store i32 %ECX_1, i32* %EAX
store i32 %RSP-40, i32* %EBP
store i32 %ECX_1, i32* %ECX
store i32 %EFLAGS_3, i32* %EFLAGS
store i32 %RSP-32, i32* %EIP
store i32 %RSP-24, i32* %ESP
store i64 %ECX_1, i64* %RAX
store i64 %RSP-40, i64* %RBP
store i64 %ECX_1, i64* %RCX
store i64 %RSP-32, i64* %RIP
store i64 %RSP-24, i64* %RSP
br label %exit_fn_400600
}

