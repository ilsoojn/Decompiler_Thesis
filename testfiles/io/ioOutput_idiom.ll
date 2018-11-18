; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_4005D0(%regset* noalias nocapture) {
entry_fn_4005D0:
  %RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
  %RIP_init = load i64, i64* %RIP_ptr
  %RIP = alloca i64
  store i64 %RIP_init, i64* %RIP
  %EIP_init = trunc i64 %RIP_init to i32
  %EIP = alloca i32
  store i32 %EIP_init, i32* %EIP
  %IP_init = trunc i64 %RIP_init to i16
  %IP = alloca i16
  store i16 %IP_init, i16* %IP
  %RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
  %RBP_init = load i64, i64* %RBP_ptr
  %RBP = alloca i64
  store i64 %RBP_init, i64* %RBP
  %RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
  %RSP_init = load i64, i64* %RSP_ptr
  %RSP = alloca i64
  store i64 %RSP_init, i64* %RSP
  %ESP_init = trunc i64 %RSP_init to i32
  %ESP = alloca i32
  store i32 %ESP_init, i32* %ESP
  %SP_init = trunc i64 %RSP_init to i16
  %SP = alloca i16
  store i16 %SP_init, i16* %SP
  %SPL_init = trunc i64 %RSP_init to i8
  %SPL = alloca i8
  store i8 %SPL_init, i8* %SPL
  %EBP_init = trunc i64 %RBP_init to i32
  %EBP = alloca i32
  store i32 %EBP_init, i32* %EBP
  %BP_init = trunc i64 %RBP_init to i16
  %BP = alloca i16
  store i16 %BP_init, i16* %BP
  %BPL_init = trunc i64 %RBP_init to i8
  %BPL = alloca i8
  store i8 %BPL_init, i8* %BPL
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
  %RDI_init = load i64, i64* %RDI_ptr
  %RDI = alloca i64
  store i64 %RDI_init, i64* %RDI
  %EDI_init = trunc i64 %RDI_init to i32
  %EDI = alloca i32
  store i32 %EDI_init, i32* %EDI
  %DI_init = trunc i64 %RDI_init to i16
  %DI = alloca i16
  store i16 %DI_init, i16* %DI
  %DIL_init = trunc i64 %RDI_init to i8
  %DIL = alloca i8
  store i8 %DIL_init, i8* %DIL
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
  %RSI_init = load i64, i64* %RSI_ptr
  %RSI = alloca i64
  store i64 %RSI_init, i64* %RSI
  %ESI_init = trunc i64 %RSI_init to i32
  %ESI = alloca i32
  store i32 %ESI_init, i32* %ESI
  %SI_init = trunc i64 %RSI_init to i16
  %SI = alloca i16
  store i16 %SI_init, i16* %SI
  %SIL_init = trunc i64 %RSI_init to i8
  %SIL = alloca i8
  store i8 %SIL_init, i8* %SIL
  %RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
  %RCX_init = load i64, i64* %RCX_ptr
  %RCX = alloca i64
  store i64 %RCX_init, i64* %RCX
  %ECX_init = trunc i64 %RCX_init to i32
  %ECX = alloca i32
  store i32 %ECX_init, i32* %ECX
  %CX_init = trunc i64 %RCX_init to i16
  %CX = alloca i16
  store i16 %CX_init, i16* %CX
  %CL_init = trunc i64 %RCX_init to i8
  %CL = alloca i8
  store i8 %CL_init, i8* %CL
  %1 = lshr i64 %RCX_init, 8
  %CH_init = trunc i64 %1 to i8
  %CH = alloca i8
  store i8 %CH_init, i8* %CH
  %2 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %2 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
  br label %bb_4005D0

exit_fn_4005D0:                                   ; preds = %bb_4005D0
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
  %10 = load i64, i64* %RSI
  store i64 %10, i64* %RSI_ptr
  %11 = load i64, i64* %RSP
  store i64 %11, i64* %RSP_ptr
  ret void

bb_4005D0:                                        ; preds = %entry_fn_4005D0
  %RIP_1 = add i64 4195792, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %12 = sub i64 %RSP_0, 8
  %13 = inttoptr i64 %12 to i64*
  store i64 %RBP_0, i64* %13, align 1
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %RIP_2 = add i64 %RIP_1, 3
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %EBP_0 = trunc i64 %RSP_1 to i32
  %BP_0 = trunc i64 %RSP_1 to i16
  %BPL_0 = trunc i64 %RSP_1 to i8
  %RIP_3 = add i64 %RIP_2, 4
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RSP_2 = sub i64 %RSP_1, 64
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 10
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %RIP_5 = add i64 %RIP_4, 7
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %14 = add i64 %RSP_1, -4
  %15 = inttoptr i64 %14 to i32*
  store i32 0, i32* %15, align 1
  %RIP_6 = add i64 %RIP_5, 5
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RSP_3 = sub i64 %RSP_2, 8
  %16 = inttoptr i64 %RSP_3 to i64*
  store i64 4195822, i64* %16
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i16 1732, i16* %DI
  store i8 -60, i8* %DIL
  store i32 %EBP_0, i32* %EBP
  store i32 4196036, i32* %EDI
  %ZF_0 = icmp eq i64 %RSP_2, 0
  %SF_0 = icmp slt i64 %RSP_2, 0
  %17 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP_1, i64 64)
  %OF_0 = extractvalue { i64, i1 } %17, 1
  %18 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP_1, i64 64)
  %CF_0 = extractvalue { i64, i1 } %18, 1
  %19 = trunc i64 %RSP_2 to i8
  %20 = call i8 @llvm.ctpop.i8(i8 %19)
  %21 = trunc i8 %20 to i1
  %PF_0 = icmp eq i1 %21, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %22 = zext i1 %CF_0 to i32
  %23 = shl i32 %22, 0
  %24 = or i32 %23, %CtlSysEFLAGS_0
  %25 = zext i1 %PF_0 to i32
  %26 = shl i32 %25, 2
  %27 = or i32 %26, %24
  %28 = zext i1 false to i32
  %29 = shl i32 %28, 4
  %30 = or i32 %29, %27
  %31 = zext i1 %ZF_0 to i32
  %32 = shl i32 %31, 6
  %33 = or i32 %32, %30
  %34 = zext i1 %SF_0 to i32
  %35 = shl i32 %34, 7
  %36 = or i32 %35, %33
  %37 = zext i1 %OF_0 to i32
  %38 = shl i32 %37, 11
  %EFLAGS_1 = or i32 %38, %36
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_5, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_5, i16* %IP
  store i64 %RSP_1, i64* %RBP
  store i64 4196036, i64* %RDI
  store i64 %RIP_6, i64* %RIP
  store i64 %RSP_3, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  %39 = load i32, i32* %CtlSysEFLAGS
  store i32 %39, i32* %CtlSysEFLAGS_ptr
  %40 = load i32, i32* %EFLAGS
  store i32 %40, i32* %EFLAGS_ptr
  %41 = load i64, i64* %RAX
  store i64 %41, i64* %RAX_ptr
  %42 = load i64, i64* %RBP
  store i64 %42, i64* %RBP_ptr
  %43 = load i64, i64* %RCX
  store i64 %43, i64* %RCX_ptr
  %44 = load i64, i64* %RDI
  store i64 %44, i64* %RDI_ptr
  %45 = load i64, i64* %RIP
  store i64 %45, i64* %RIP_ptr
  %46 = load i64, i64* %RSI
  store i64 %46, i64* %RSI_ptr
  %47 = load i64, i64* %RSP
  store i64 %47, i64* %RSP_ptr
  call void @fn_4004A0(%regset* %0)
  %48 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %48, i32* %CtlSysEFLAGS
  %49 = load i32, i32* %EFLAGS_ptr
  store i32 %49, i32* %EFLAGS
  %50 = load i64, i64* %RAX_ptr
  store i64 %50, i64* %RAX
  %51 = load i64, i64* %RBP_ptr
  store i64 %51, i64* %RBP
  %52 = load i64, i64* %RCX_ptr
  store i64 %52, i64* %RCX
  %53 = load i64, i64* %RDI_ptr
  store i64 %53, i64* %RDI
  %54 = load i64, i64* %RIP_ptr
  store i64 %54, i64* %RIP
  %55 = load i64, i64* %RSI_ptr
  store i64 %55, i64* %RSI
  %56 = load i64, i64* %RSP_ptr
  store i64 %56, i64* %RSP
  %RIP_7 = load i64, i64* %RIP
  %RIP_8 = add i64 %RIP_7, 4
  %EIP_6 = trunc i64 %RIP_8 to i32
  %IP_6 = trunc i64 %RIP_8 to i16
  %RBP_1 = load i64, i64* %RBP
  %RDI_1 = add i64 %RBP_1, -48
  %EDI_1 = trunc i64 %RDI_1 to i32
  %DI_1 = trunc i64 %RDI_1 to i16
  %DIL_1 = trunc i64 %RDI_1 to i8
  %RIP_9 = add i64 %RIP_8, 3
  %EIP_7 = trunc i64 %RIP_9 to i32
  %IP_7 = trunc i64 %RIP_9 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %57 = add i64 %RBP_1, -52
  %58 = inttoptr i64 %57 to i32*
  store i32 %EAX_0, i32* %58, align 1
  %RIP_10 = add i64 %RIP_9, 2
  %EIP_8 = trunc i64 %RIP_10 to i32
  %IP_8 = trunc i64 %RIP_10 to i16
  %AX_0 = trunc i64 %RAX_0 to i16
  %59 = and i16 %AX_0, -256
  %AX_1 = or i16 0, %59
  %60 = and i32 %EAX_0, -256
  %EAX_1 = or i32 0, %60
  %61 = and i64 %RAX_0, -256
  %RAX_1 = or i64 0, %61
  %RIP_11 = add i64 %RIP_10, 5
  %EIP_9 = trunc i64 %RIP_11 to i32
  %IP_9 = trunc i64 %RIP_11 to i16
  %RSP_4 = load i64, i64* %RSP
  %RSP_5 = sub i64 %RSP_4, 8
  %62 = inttoptr i64 %RSP_5 to i64*
  store i64 4195836, i64* %62
  %ESP_3 = trunc i64 %RSP_5 to i32
  %SP_3 = trunc i64 %RSP_5 to i16
  %SPL_3 = trunc i64 %RSP_5 to i8
  store i8 0, i8* %AL
  store i16 %AX_1, i16* %AX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i16 %DI_1, i16* %DI
  store i8 %DIL_1, i8* %DIL
  store i32 %EAX_1, i32* %EAX
  store i32 %EDI_1, i32* %EDI
  store i32 %EIP_9, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_9, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 %RDI_1, i64* %RDI
  store i64 %RIP_11, i64* %RIP
  store i64 %RSP_5, i64* %RSP
  store i16 %SP_3, i16* %SP
  store i8 %SPL_3, i8* %SPL
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
  %70 = load i64, i64* %RSI
  store i64 %70, i64* %RSI_ptr
  %71 = load i64, i64* %RSP
  store i64 %71, i64* %RSP_ptr
  call void @fn_4004C0(%regset* %0)
  %72 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %72, i32* %CtlSysEFLAGS
  %73 = load i32, i32* %EFLAGS_ptr
  store i32 %73, i32* %EFLAGS
  %74 = load i64, i64* %RAX_ptr
  store i64 %74, i64* %RAX
  %75 = load i64, i64* %RBP_ptr
  store i64 %75, i64* %RBP
  %76 = load i64, i64* %RCX_ptr
  store i64 %76, i64* %RCX
  %77 = load i64, i64* %RDI_ptr
  store i64 %77, i64* %RDI
  %78 = load i64, i64* %RIP_ptr
  store i64 %78, i64* %RIP
  %79 = load i64, i64* %RSI_ptr
  store i64 %79, i64* %RSI
  %80 = load i64, i64* %RSP_ptr
  store i64 %80, i64* %RSP
  %RIP_12 = load i64, i64* %RIP
  %RIP_13 = add i64 %RIP_12, 10
  %EIP_10 = trunc i64 %RIP_13 to i32
  %IP_10 = trunc i64 %RIP_13 to i16
  %RIP_14 = add i64 %RIP_13, 3
  %EIP_11 = trunc i64 %RIP_14 to i32
  %IP_11 = trunc i64 %RIP_14 to i16
  %RAX_2 = load i64, i64* %RAX
  %EAX_2 = trunc i64 %RAX_2 to i32
  %RBP_2 = load i64, i64* %RBP
  %81 = add i64 %RBP_2, -56
  %82 = inttoptr i64 %81 to i32*
  store i32 %EAX_2, i32* %82, align 1
  %RIP_15 = add i64 %RIP_14, 2
  %EIP_12 = trunc i64 %RIP_15 to i32
  %IP_12 = trunc i64 %RIP_15 to i16
  %AX_2 = trunc i64 %RAX_2 to i16
  %83 = and i16 %AX_2, -256
  %AX_3 = or i16 0, %83
  %84 = and i32 %EAX_2, -256
  %EAX_3 = or i32 0, %84
  %85 = and i64 %RAX_2, -256
  %RAX_3 = or i64 0, %85
  %RIP_16 = add i64 %RIP_15, 5
  %EIP_13 = trunc i64 %RIP_16 to i32
  %IP_13 = trunc i64 %RIP_16 to i16
  %RSP_6 = load i64, i64* %RSP
  %RSP_7 = sub i64 %RSP_6, 8
  %86 = inttoptr i64 %RSP_7 to i64*
  store i64 4195856, i64* %86
  %ESP_4 = trunc i64 %RSP_7 to i32
  %SP_4 = trunc i64 %RSP_7 to i16
  %SPL_4 = trunc i64 %RSP_7 to i8
  store i8 0, i8* %AL
  store i16 %AX_3, i16* %AX
  store i16 1763, i16* %DI
  store i8 -29, i8* %DIL
  store i32 %EAX_3, i32* %EAX
  store i32 4196067, i32* %EDI
  store i32 %EIP_13, i32* %EIP
  store i32 %ESP_4, i32* %ESP
  store i16 %IP_13, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 4196067, i64* %RDI
  store i64 %RIP_16, i64* %RIP
  store i64 %RSP_7, i64* %RSP
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  %87 = load i32, i32* %CtlSysEFLAGS
  store i32 %87, i32* %CtlSysEFLAGS_ptr
  %88 = load i32, i32* %EFLAGS
  store i32 %88, i32* %EFLAGS_ptr
  %89 = load i64, i64* %RAX
  store i64 %89, i64* %RAX_ptr
  %90 = load i64, i64* %RBP
  store i64 %90, i64* %RBP_ptr
  %91 = load i64, i64* %RCX
  store i64 %91, i64* %RCX_ptr
  %92 = load i64, i64* %RDI
  store i64 %92, i64* %RDI_ptr
  %93 = load i64, i64* %RIP
  store i64 %93, i64* %RIP_ptr
  %94 = load i64, i64* %RSI
  store i64 %94, i64* %RSI_ptr
  %95 = load i64, i64* %RSP
  store i64 %95, i64* %RSP_ptr
  call void @fn_4004B0(%regset* %0)
  %96 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %96, i32* %CtlSysEFLAGS
  %97 = load i32, i32* %EFLAGS_ptr
  store i32 %97, i32* %EFLAGS
  %98 = load i64, i64* %RAX_ptr
  store i64 %98, i64* %RAX
  %99 = load i64, i64* %RBP_ptr
  store i64 %99, i64* %RBP
  %100 = load i64, i64* %RCX_ptr
  store i64 %100, i64* %RCX
  %101 = load i64, i64* %RDI_ptr
  store i64 %101, i64* %RDI
  %102 = load i64, i64* %RIP_ptr
  store i64 %102, i64* %RIP
  %103 = load i64, i64* %RSI_ptr
  store i64 %103, i64* %RSI
  %104 = load i64, i64* %RSP_ptr
  store i64 %104, i64* %RSP
  %RIP_17 = load i64, i64* %RIP
  %RIP_18 = add i64 %RIP_17, 10
  %EIP_14 = trunc i64 %RIP_18 to i32
  %IP_14 = trunc i64 %RIP_18 to i16
  %RIP_19 = add i64 %RIP_18, 4
  %EIP_15 = trunc i64 %RIP_19 to i32
  %IP_15 = trunc i64 %RIP_19 to i16
  %RBP_3 = load i64, i64* %RBP
  %RSI_0 = add i64 %RBP_3, -8
  %ESI_0 = trunc i64 %RSI_0 to i32
  %SI_0 = trunc i64 %RSI_0 to i16
  %SIL_0 = trunc i64 %RSI_0 to i8
  %RIP_20 = add i64 %RIP_19, 3
  %EIP_16 = trunc i64 %RIP_20 to i32
  %IP_16 = trunc i64 %RIP_20 to i16
  %RAX_4 = load i64, i64* %RAX
  %EAX_4 = trunc i64 %RAX_4 to i32
  %105 = add i64 %RBP_3, -60
  %106 = inttoptr i64 %105 to i32*
  store i32 %EAX_4, i32* %106, align 1
  %RIP_21 = add i64 %RIP_20, 2
  %EIP_17 = trunc i64 %RIP_21 to i32
  %IP_17 = trunc i64 %RIP_21 to i16
  %AX_4 = trunc i64 %RAX_4 to i16
  %107 = and i16 %AX_4, -256
  %AX_5 = or i16 0, %107
  %108 = and i32 %EAX_4, -256
  %EAX_5 = or i32 0, %108
  %109 = and i64 %RAX_4, -256
  %RAX_5 = or i64 0, %109
  %RIP_22 = add i64 %RIP_21, 5
  %EIP_18 = trunc i64 %RIP_22 to i32
  %IP_18 = trunc i64 %RIP_22 to i16
  %RSP_8 = load i64, i64* %RSP
  %RSP_9 = sub i64 %RSP_8, 8
  %110 = inttoptr i64 %RSP_9 to i64*
  store i64 4195880, i64* %110
  %ESP_5 = trunc i64 %RSP_9 to i32
  %SP_5 = trunc i64 %RSP_9 to i16
  %SPL_5 = trunc i64 %RSP_9 to i8
  store i8 0, i8* %AL
  store i16 %AX_5, i16* %AX
  store i16 1785, i16* %DI
  store i8 -7, i8* %DIL
  store i32 %EAX_5, i32* %EAX
  store i32 4196089, i32* %EDI
  store i32 %EIP_18, i32* %EIP
  store i32 %ESI_0, i32* %ESI
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_18, i16* %IP
  store i64 %RAX_5, i64* %RAX
  store i64 %RBP_3, i64* %RBP
  store i64 4196089, i64* %RDI
  store i64 %RIP_22, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_9, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_5, i16* %SP
  store i8 %SPL_5, i8* %SPL
  %111 = load i32, i32* %CtlSysEFLAGS
  store i32 %111, i32* %CtlSysEFLAGS_ptr
  %112 = load i32, i32* %EFLAGS
  store i32 %112, i32* %EFLAGS_ptr
  %113 = load i64, i64* %RAX
  store i64 %113, i64* %RAX_ptr
  %114 = load i64, i64* %RBP
  store i64 %114, i64* %RBP_ptr
  %115 = load i64, i64* %RCX
  store i64 %115, i64* %RCX_ptr
  %116 = load i64, i64* %RDI
  store i64 %116, i64* %RDI_ptr
  %117 = load i64, i64* %RIP
  store i64 %117, i64* %RIP_ptr
  %118 = load i64, i64* %RSI
  store i64 %118, i64* %RSI_ptr
  %119 = load i64, i64* %RSP
  store i64 %119, i64* %RSP_ptr
  call void @fn_4004D0(%regset* %0)
  %120 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %120, i32* %CtlSysEFLAGS
  %121 = load i32, i32* %EFLAGS_ptr
  store i32 %121, i32* %EFLAGS
  %122 = load i64, i64* %RAX_ptr
  store i64 %122, i64* %RAX
  %123 = load i64, i64* %RBP_ptr
  store i64 %123, i64* %RBP
  %124 = load i64, i64* %RCX_ptr
  store i64 %124, i64* %RCX
  %125 = load i64, i64* %RDI_ptr
  store i64 %125, i64* %RDI
  %126 = load i64, i64* %RIP_ptr
  store i64 %126, i64* %RIP
  %127 = load i64, i64* %RSI_ptr
  store i64 %127, i64* %RSI
  %128 = load i64, i64* %RSP_ptr
  store i64 %128, i64* %RSP
  %RIP_23 = load i64, i64* %RIP
  %RIP_24 = add i64 %RIP_23, 2
  %EIP_19 = trunc i64 %RIP_24 to i32
  %IP_19 = trunc i64 %RIP_24 to i16
  %RCX_0 = load i64, i64* %RCX
  %ECX_0 = trunc i64 %RCX_0 to i32
  %ECX_1 = xor i32 %ECX_0, %ECX_0
  %RCX_1 = zext i32 %ECX_1 to i64
  %CX_0 = trunc i32 %ECX_1 to i16
  %CL_0 = trunc i32 %ECX_1 to i8
  %129 = lshr i32 %ECX_1, 8
  %CH_0 = trunc i32 %129 to i8
  %EFLAGS_2 = load i32, i32* %EFLAGS
  %RIP_25 = add i64 %RIP_24, 3
  %EIP_20 = trunc i64 %RIP_25 to i32
  %IP_20 = trunc i64 %RIP_25 to i16
  %RAX_6 = load i64, i64* %RAX
  %EAX_6 = trunc i64 %RAX_6 to i32
  %RBP_4 = load i64, i64* %RBP
  %130 = add i64 %RBP_4, -64
  %131 = inttoptr i64 %130 to i32*
  store i32 %EAX_6, i32* %131, align 1
  %RIP_26 = add i64 %RIP_25, 2
  %EIP_21 = trunc i64 %RIP_26 to i32
  %IP_21 = trunc i64 %RIP_26 to i16
  %RAX_7 = zext i32 %ECX_1 to i64
  %AX_6 = trunc i32 %ECX_1 to i16
  %AL_3 = trunc i32 %ECX_1 to i8
  %132 = lshr i32 %ECX_1, 8
  %AH_0 = trunc i32 %132 to i8
  %RIP_27 = add i64 %RIP_26, 4
  %EIP_22 = trunc i64 %RIP_27 to i32
  %IP_22 = trunc i64 %RIP_27 to i16
  %RSP_10 = load i64, i64* %RSP
  %RSP_11 = add i64 %RSP_10, 64
  %ESP_6 = trunc i64 %RSP_11 to i32
  %SP_6 = trunc i64 %RSP_11 to i16
  %SPL_6 = trunc i64 %RSP_11 to i8
  %RIP_28 = add i64 %RIP_27, 1
  %EIP_23 = trunc i64 %RIP_28 to i32
  %IP_23 = trunc i64 %RIP_28 to i16
  %RSP_12 = add i64 %RSP_11, 8
  %ESP_7 = trunc i64 %RSP_12 to i32
  %SP_7 = trunc i64 %RSP_12 to i16
  %SPL_7 = trunc i64 %RSP_12 to i8
  %133 = sub i64 %RSP_12, 8
  %134 = inttoptr i64 %133 to i64*
  %RBP_5 = load i64, i64* %134, align 1
  %EBP_1 = trunc i64 %RBP_5 to i32
  %BP_1 = trunc i64 %RBP_5 to i16
  %BPL_1 = trunc i64 %RBP_5 to i8
  %RIP_29 = add i64 %RIP_28, 1
  %EIP_24 = trunc i64 %RIP_29 to i32
  %IP_24 = trunc i64 %RIP_29 to i16
  %RSP_13 = add i64 %RSP_12, 8
  %135 = inttoptr i64 %RSP_12 to i64*
  %RIP_30 = load i64, i64* %135
  %ESP_8 = trunc i64 %RSP_13 to i32
  %SP_8 = trunc i64 %RSP_13 to i16
  %SPL_8 = trunc i64 %RSP_13 to i8
  %EIP_25 = trunc i64 %RIP_30 to i32
  %IP_25 = trunc i64 %RIP_30 to i16
  %ZF_1 = icmp eq i64 %RSP_11, 0
  %SF_1 = icmp slt i64 %RSP_11, 0
  %136 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_10, i64 64)
  %OF_1 = extractvalue { i64, i1 } %136, 1
  %137 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_10, i64 64)
  %CF_1 = extractvalue { i64, i1 } %137, 1
  %138 = trunc i64 %RSP_11 to i8
  %139 = call i8 @llvm.ctpop.i8(i8 %138)
  %140 = trunc i8 %139 to i1
  %PF_1 = icmp eq i1 %140, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %141 = zext i1 %CF_1 to i32
  %142 = shl i32 %141, 0
  %143 = or i32 %142, %CtlSysEFLAGS_1
  %144 = zext i1 %PF_1 to i32
  %145 = shl i32 %144, 2
  %146 = or i32 %145, %143
  %147 = zext i1 false to i32
  %148 = shl i32 %147, 4
  %149 = or i32 %148, %146
  %150 = zext i1 %ZF_1 to i32
  %151 = shl i32 %150, 6
  %152 = or i32 %151, %149
  %153 = zext i1 %SF_1 to i32
  %154 = shl i32 %153, 7
  %155 = or i32 %154, %152
  %156 = zext i1 %OF_1 to i32
  %157 = shl i32 %156, 11
  %EFLAGS_3 = or i32 %157, %155
  store i8 %AH_0, i8* %AH
  store i8 %AL_3, i8* %AL
  store i16 %AX_6, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 %CH_0, i8* %CH
  store i8 %CL_0, i8* %CL
  store i16 %CX_0, i16* %CX
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 %ECX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 %ECX_1, i32* %ECX
  store i32 %EFLAGS_3, i32* %EFLAGS
  store i32 %EIP_25, i32* %EIP
  store i32 %ESP_8, i32* %ESP
  store i16 %IP_25, i16* %IP
  store i64 %RAX_7, i64* %RAX
  store i64 %RBP_5, i64* %RBP
  store i64 %RCX_1, i64* %RCX
  store i64 %RIP_30, i64* %RIP
  store i64 %RSP_13, i64* %RSP
  store i16 %SP_8, i16* %SP
  store i8 %SPL_8, i8* %SPL
  br label %exit_fn_4005D0
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

define void @fn_4004A0(%regset* noalias nocapture) {
entry_fn_4004A0:
  %RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
  %RIP_init = load i64, i64* %RIP_ptr
  %RIP = alloca i64
  store i64 %RIP_init, i64* %RIP
  %EIP_init = trunc i64 %RIP_init to i32
  %EIP = alloca i32
  store i32 %EIP_init, i32* %EIP
  %IP_init = trunc i64 %RIP_init to i16
  %IP = alloca i16
  store i16 %IP_init, i16* %IP
  br label %bb_4004A0

exit_fn_4004A0:                                   ; preds = %bb_4004A0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  ret void

bb_4004A0:                                        ; preds = %entry_fn_4004A0
  %RIP_1 = add i64 4195488, 6
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %2 = add i64 %RIP_1, 2100082
  %3 = inttoptr i64 %2 to i64*
  %RIP_2 = load i64, i64* %3, align 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %4 = inttoptr i64 %RIP_2 to i8*
  %5 = call i8* @llvm.dc.translate.at(i8* %4)
  %6 = bitcast i8* %5 to void (%regset*)*
  store i32 %EIP_1, i32* %EIP
  store i16 %IP_1, i16* %IP
  store i64 %RIP_2, i64* %RIP
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  call void %6(%regset* %0)
  %8 = load i64, i64* %RIP_ptr
  store i64 %8, i64* %RIP
  br label %exit_fn_4004A0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define void @fn_4004C0(%regset* noalias nocapture) {
entry_fn_4004C0:
  %RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
  %RIP_init = load i64, i64* %RIP_ptr
  %RIP = alloca i64
  store i64 %RIP_init, i64* %RIP
  %EIP_init = trunc i64 %RIP_init to i32
  %EIP = alloca i32
  store i32 %EIP_init, i32* %EIP
  %IP_init = trunc i64 %RIP_init to i16
  %IP = alloca i16
  store i16 %IP_init, i16* %IP
  br label %bb_4004C0

exit_fn_4004C0:                                   ; preds = %bb_4004C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  ret void

bb_4004C0:                                        ; preds = %entry_fn_4004C0
  %RIP_1 = add i64 4195520, 6
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %2 = add i64 %RIP_1, 2100066
  %3 = inttoptr i64 %2 to i64*
  %RIP_2 = load i64, i64* %3, align 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %4 = inttoptr i64 %RIP_2 to i8*
  %5 = call i8* @llvm.dc.translate.at(i8* %4)
  %6 = bitcast i8* %5 to void (%regset*)*
  store i32 %EIP_1, i32* %EIP
  store i16 %IP_1, i16* %IP
  store i64 %RIP_2, i64* %RIP
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  call void %6(%regset* %0)
  %8 = load i64, i64* %RIP_ptr
  store i64 %8, i64* %RIP
  br label %exit_fn_4004C0
}

define void @fn_4004B0(%regset* noalias nocapture) {
entry_fn_4004B0:
  %RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
  %RIP_init = load i64, i64* %RIP_ptr
  %RIP = alloca i64
  store i64 %RIP_init, i64* %RIP
  %EIP_init = trunc i64 %RIP_init to i32
  %EIP = alloca i32
  store i32 %EIP_init, i32* %EIP
  %IP_init = trunc i64 %RIP_init to i16
  %IP = alloca i16
  store i16 %IP_init, i16* %IP
  br label %bb_4004B0

exit_fn_4004B0:                                   ; preds = %bb_4004B0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  ret void

bb_4004B0:                                        ; preds = %entry_fn_4004B0
  %RIP_1 = add i64 4195504, 6
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %2 = add i64 %RIP_1, 2100074
  %3 = inttoptr i64 %2 to i64*
  %RIP_2 = load i64, i64* %3, align 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %4 = inttoptr i64 %RIP_2 to i8*
  %5 = call i8* @llvm.dc.translate.at(i8* %4)
  %6 = bitcast i8* %5 to void (%regset*)*
  store i32 %EIP_1, i32* %EIP
  store i16 %IP_1, i16* %IP
  store i64 %RIP_2, i64* %RIP
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  call void %6(%regset* %0)
  %8 = load i64, i64* %RIP_ptr
  store i64 %8, i64* %RIP
  br label %exit_fn_4004B0
}

define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
  %RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
  %RIP_init = load i64, i64* %RIP_ptr
  %RIP = alloca i64
  store i64 %RIP_init, i64* %RIP
  %EIP_init = trunc i64 %RIP_init to i32
  %EIP = alloca i32
  store i32 %EIP_init, i32* %EIP
  %IP_init = trunc i64 %RIP_init to i16
  %IP = alloca i16
  store i16 %IP_init, i16* %IP
  br label %bb_4004D0

exit_fn_4004D0:                                   ; preds = %bb_4004D0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  ret void

bb_4004D0:                                        ; preds = %entry_fn_4004D0
  %RIP_1 = add i64 4195536, 6
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %2 = add i64 %RIP_1, 2100058
  %3 = inttoptr i64 %2 to i64*
  %RIP_2 = load i64, i64* %3, align 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %4 = inttoptr i64 %RIP_2 to i8*
  %5 = call i8* @llvm.dc.translate.at(i8* %4)
  %6 = bitcast i8* %5 to void (%regset*)*
  store i32 %EIP_1, i32* %EIP
  store i16 %IP_1, i16* %IP
  store i64 %RIP_2, i64* %RIP
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  call void %6(%regset* %0)
  %8 = load i64, i64* %RIP_ptr
  store i64 %8, i64* %RIP
  br label %exit_fn_4004D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_4005D0(%regset* %3)
  %6 = call i32 @main_fini_regset(%regset* %3)
  ret i32 %6
}
