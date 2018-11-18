; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400600(%regset* noalias nocapture) {
entry_fn_400600:
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
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
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
  %RIP_1 = add i64 4195840, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %11 = sub i64 %RSP_0, 8
  %12 = inttoptr i64 %11 to i64*
  store i64 %RBP_0, i64* %12, align 1
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
  %RSP_2 = sub i64 %RSP_1, 16
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
  %13 = add i64 %RSP_1, -4
  %14 = inttoptr i64 %13 to i32*
  store i32 0, i32* %14, align 1
  %RIP_6 = add i64 %RIP_5, 2
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RAX_0 = load i64, i64* %RAX
  %AX_0 = trunc i64 %RAX_0 to i16
  %15 = and i16 %AX_0, -256
  %AX_1 = or i16 0, %15
  %EAX_0 = trunc i64 %RAX_0 to i32
  %16 = and i32 %EAX_0, -256
  %EAX_1 = or i32 0, %16
  %17 = and i64 %RAX_0, -256
  %RAX_1 = or i64 0, %17
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RSP_3 = sub i64 %RSP_2, 8
  %18 = inttoptr i64 %RSP_3 to i64*
  store i64 4195872, i64* %18
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  store i8 0, i8* %AL
  store i16 %AX_1, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i16 1796, i16* %DI
  store i8 4, i8* %DIL
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 4196100, i32* %EDI
  %ZF_0 = icmp eq i64 %RSP_2, 0
  %SF_0 = icmp slt i64 %RSP_2, 0
  %19 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP_1, i64 16)
  %OF_0 = extractvalue { i64, i1 } %19, 1
  %20 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP_1, i64 16)
  %CF_0 = extractvalue { i64, i1 } %20, 1
  %21 = trunc i64 %RSP_2 to i8
  %22 = call i8 @llvm.ctpop.i8(i8 %21)
  %23 = trunc i8 %22 to i1
  %PF_0 = icmp eq i1 %23, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %24 = zext i1 %CF_0 to i32
  %25 = shl i32 %24, 0
  %26 = or i32 %25, %CtlSysEFLAGS_0
  %27 = zext i1 %PF_0 to i32
  %28 = shl i32 %27, 2
  %29 = or i32 %28, %26
  %30 = zext i1 false to i32
  %31 = shl i32 %30, 4
  %32 = or i32 %31, %29
  %33 = zext i1 %ZF_0 to i32
  %34 = shl i32 %33, 6
  %35 = or i32 %34, %32
  %36 = zext i1 %SF_0 to i32
  %37 = shl i32 %36, 7
  %38 = or i32 %37, %35
  %39 = zext i1 %OF_0 to i32
  %40 = shl i32 %39, 11
  %EFLAGS_1 = or i32 %40, %38
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_6, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_6, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4196100, i64* %RDI
  store i64 %RIP_7, i64* %RIP
  store i64 %RSP_3, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
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
  %RIP_8 = load i64, i64* %RIP
  %RIP_9 = add i64 %RIP_8, 3
  %EIP_7 = trunc i64 %RIP_9 to i32
  %IP_7 = trunc i64 %RIP_9 to i16
  %RAX_2 = load i64, i64* %RAX
  %EAX_2 = trunc i64 %RAX_2 to i32
  %RBP_1 = load i64, i64* %RBP
  %57 = add i64 %RBP_1, -8
  %58 = inttoptr i64 %57 to i32*
  store i32 %EAX_2, i32* %58, align 1
  %RIP_10 = add i64 %RIP_9, 2
  %EIP_8 = trunc i64 %RIP_10 to i32
  %IP_8 = trunc i64 %RIP_10 to i16
  %AX_2 = trunc i64 %RAX_2 to i16
  %59 = and i16 %AX_2, -256
  %AX_3 = or i16 0, %59
  %60 = and i32 %EAX_2, -256
  %EAX_3 = or i32 0, %60
  %61 = and i64 %RAX_2, -256
  %RAX_3 = or i64 0, %61
  %RIP_11 = add i64 %RIP_10, 5
  %EIP_9 = trunc i64 %RIP_11 to i32
  %IP_9 = trunc i64 %RIP_11 to i16
  %RSP_4 = load i64, i64* %RSP
  %RSP_5 = sub i64 %RSP_4, 8
  %62 = inttoptr i64 %RSP_5 to i64*
  store i64 4195882, i64* %62
  %ESP_3 = trunc i64 %RSP_5 to i32
  %SP_3 = trunc i64 %RSP_5 to i16
  %SPL_3 = trunc i64 %RSP_5 to i8
  store i8 0, i8* %AL
  store i16 %AX_3, i16* %AX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EAX_3, i32* %EAX
  store i32 %EIP_9, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_9, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_1, i64* %RBP
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
  %RIP_12 = load i64, i64* %RIP
  %RIP_13 = add i64 %RIP_12, 2
  %EIP_10 = trunc i64 %RIP_13 to i32
  %IP_10 = trunc i64 %RIP_13 to i16
  %RAX_4 = load i64, i64* %RAX
  %AX_4 = trunc i64 %RAX_4 to i16
  %79 = and i16 %AX_4, -256
  %AX_5 = or i16 0, %79
  %EAX_4 = trunc i64 %RAX_4 to i32
  %80 = and i32 %EAX_4, -256
  %EAX_5 = or i32 0, %80
  %81 = and i64 %RAX_4, -256
  %RAX_5 = or i64 0, %81
  %RIP_14 = add i64 %RIP_13, 5
  %EIP_11 = trunc i64 %RIP_14 to i32
  %IP_11 = trunc i64 %RIP_14 to i16
  %RSP_6 = load i64, i64* %RSP
  %RSP_7 = sub i64 %RSP_6, 8
  %82 = inttoptr i64 %RSP_7 to i64*
  store i64 4195889, i64* %82
  %ESP_4 = trunc i64 %RSP_7 to i32
  %SP_4 = trunc i64 %RSP_7 to i16
  %SPL_4 = trunc i64 %RSP_7 to i8
  store i8 0, i8* %AL
  store i16 %AX_5, i16* %AX
  store i32 %EAX_5, i32* %EAX
  store i32 %EIP_11, i32* %EIP
  store i32 %ESP_4, i32* %ESP
  store i16 %IP_11, i16* %IP
  store i64 %RAX_5, i64* %RAX
  store i64 %RIP_14, i64* %RIP
  store i64 %RSP_7, i64* %RSP
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
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
  %RIP_15 = load i64, i64* %RIP
  %RIP_16 = add i64 %RIP_15, 10
  %EIP_12 = trunc i64 %RIP_16 to i32
  %IP_12 = trunc i64 %RIP_16 to i16
  %RIP_17 = add i64 %RIP_16, 2
  %EIP_13 = trunc i64 %RIP_17 to i32
  %IP_13 = trunc i64 %RIP_17 to i16
  %RAX_6 = load i64, i64* %RAX
  %AX_6 = trunc i64 %RAX_6 to i16
  %99 = and i16 %AX_6, -256
  %AX_7 = or i16 0, %99
  %EAX_6 = trunc i64 %RAX_6 to i32
  %100 = and i32 %EAX_6, -256
  %EAX_7 = or i32 0, %100
  %101 = and i64 %RAX_6, -256
  %RAX_7 = or i64 0, %101
  %RIP_18 = add i64 %RIP_17, 5
  %EIP_14 = trunc i64 %RIP_18 to i32
  %IP_14 = trunc i64 %RIP_18 to i16
  %RSP_8 = load i64, i64* %RSP
  %RSP_9 = sub i64 %RSP_8, 8
  %102 = inttoptr i64 %RSP_9 to i64*
  store i64 4195906, i64* %102
  %ESP_5 = trunc i64 %RSP_9 to i32
  %SP_5 = trunc i64 %RSP_9 to i16
  %SPL_5 = trunc i64 %RSP_9 to i8
  store i8 0, i8* %AL
  store i16 %AX_7, i16* %AX
  store i16 1808, i16* %DI
  store i8 16, i8* %DIL
  store i32 %EAX_7, i32* %EAX
  store i32 4196112, i32* %EDI
  store i32 %EIP_14, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_14, i16* %IP
  store i64 %RAX_7, i64* %RAX
  store i64 4196112, i64* %RDI
  store i64 %RIP_18, i64* %RIP
  store i64 %RSP_9, i64* %RSP
  store i16 %SP_5, i16* %SP
  store i8 %SPL_5, i8* %SPL
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
  %RIP_19 = load i64, i64* %RIP
  %RIP_20 = add i64 %RIP_19, 2
  %EIP_15 = trunc i64 %RIP_20 to i32
  %IP_15 = trunc i64 %RIP_20 to i16
  %RCX_0 = load i64, i64* %RCX
  %ECX_0 = trunc i64 %RCX_0 to i32
  %ECX_1 = xor i32 %ECX_0, %ECX_0
  %RCX_1 = zext i32 %ECX_1 to i64
  %CX_0 = trunc i32 %ECX_1 to i16
  %CL_0 = trunc i32 %ECX_1 to i8
  %119 = lshr i32 %ECX_1, 8
  %CH_0 = trunc i32 %119 to i8
  %EFLAGS_2 = load i32, i32* %EFLAGS
  %RIP_21 = add i64 %RIP_20, 3
  %EIP_16 = trunc i64 %RIP_21 to i32
  %IP_16 = trunc i64 %RIP_21 to i16
  %RAX_8 = load i64, i64* %RAX
  %EAX_8 = trunc i64 %RAX_8 to i32
  %RBP_2 = load i64, i64* %RBP
  %120 = add i64 %RBP_2, -12
  %121 = inttoptr i64 %120 to i32*
  store i32 %EAX_8, i32* %121, align 1
  %RIP_22 = add i64 %RIP_21, 2
  %EIP_17 = trunc i64 %RIP_22 to i32
  %IP_17 = trunc i64 %RIP_22 to i16
  %RAX_9 = zext i32 %ECX_1 to i64
  %AX_8 = trunc i32 %ECX_1 to i16
  %AL_4 = trunc i32 %ECX_1 to i8
  %122 = lshr i32 %ECX_1, 8
  %AH_0 = trunc i32 %122 to i8
  %RIP_23 = add i64 %RIP_22, 4
  %EIP_18 = trunc i64 %RIP_23 to i32
  %IP_18 = trunc i64 %RIP_23 to i16
  %RSP_10 = load i64, i64* %RSP
  %RSP_11 = add i64 %RSP_10, 16
  %ESP_6 = trunc i64 %RSP_11 to i32
  %SP_6 = trunc i64 %RSP_11 to i16
  %SPL_6 = trunc i64 %RSP_11 to i8
  %RIP_24 = add i64 %RIP_23, 1
  %EIP_19 = trunc i64 %RIP_24 to i32
  %IP_19 = trunc i64 %RIP_24 to i16
  %RSP_12 = add i64 %RSP_11, 8
  %ESP_7 = trunc i64 %RSP_12 to i32
  %SP_7 = trunc i64 %RSP_12 to i16
  %SPL_7 = trunc i64 %RSP_12 to i8
  %123 = sub i64 %RSP_12, 8
  %124 = inttoptr i64 %123 to i64*
  %RBP_3 = load i64, i64* %124, align 1
  %EBP_1 = trunc i64 %RBP_3 to i32
  %BP_1 = trunc i64 %RBP_3 to i16
  %BPL_1 = trunc i64 %RBP_3 to i8
  %RIP_25 = add i64 %RIP_24, 1
  %EIP_20 = trunc i64 %RIP_25 to i32
  %IP_20 = trunc i64 %RIP_25 to i16
  %RSP_13 = add i64 %RSP_12, 8
  %125 = inttoptr i64 %RSP_12 to i64*
  %RIP_26 = load i64, i64* %125
  %ESP_8 = trunc i64 %RSP_13 to i32
  %SP_8 = trunc i64 %RSP_13 to i16
  %SPL_8 = trunc i64 %RSP_13 to i8
  %EIP_21 = trunc i64 %RIP_26 to i32
  %IP_21 = trunc i64 %RIP_26 to i16
  %ZF_1 = icmp eq i64 %RSP_11, 0
  %SF_1 = icmp slt i64 %RSP_11, 0
  %126 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_10, i64 16)
  %OF_1 = extractvalue { i64, i1 } %126, 1
  %127 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_10, i64 16)
  %CF_1 = extractvalue { i64, i1 } %127, 1
  %128 = trunc i64 %RSP_11 to i8
  %129 = call i8 @llvm.ctpop.i8(i8 %128)
  %130 = trunc i8 %129 to i1
  %PF_1 = icmp eq i1 %130, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %131 = zext i1 %CF_1 to i32
  %132 = shl i32 %131, 0
  %133 = or i32 %132, %CtlSysEFLAGS_1
  %134 = zext i1 %PF_1 to i32
  %135 = shl i32 %134, 2
  %136 = or i32 %135, %133
  %137 = zext i1 false to i32
  %138 = shl i32 %137, 4
  %139 = or i32 %138, %136
  %140 = zext i1 %ZF_1 to i32
  %141 = shl i32 %140, 6
  %142 = or i32 %141, %139
  %143 = zext i1 %SF_1 to i32
  %144 = shl i32 %143, 7
  %145 = or i32 %144, %142
  %146 = zext i1 %OF_1 to i32
  %147 = shl i32 %146, 11
  %EFLAGS_3 = or i32 %147, %145
  store i8 %AH_0, i8* %AH
  store i8 %AL_4, i8* %AL
  store i16 %AX_8, i16* %AX
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
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_8, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_9, i64* %RAX
  store i64 %RBP_3, i64* %RBP
  store i64 %RCX_1, i64* %RCX
  store i64 %RIP_26, i64* %RIP
  store i64 %RSP_13, i64* %RSP
  store i16 %SP_8, i16* %SP
  store i8 %SPL_8, i8* %SPL
  br label %exit_fn_400600
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

define void @fn_400500(%regset* noalias nocapture) {
entry_fn_400500:
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
  br label %bb_400500

exit_fn_400500:                                   ; preds = %bb_400500
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  ret void

bb_400500:                                        ; preds = %entry_fn_400500
  %RIP_1 = add i64 4195584, 6
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %2 = add i64 %RIP_1, 2099994
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
  br label %exit_fn_400500
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define void @fn_400650(%regset* noalias nocapture) {
entry_fn_400650:
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
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %EAX_init = trunc i64 %RAX_init to i32
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
  %RIP_1 = add i64 4195920, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %8 = sub i64 %RSP_0, 8
  %9 = inttoptr i64 %8 to i64*
  store i64 %RBP_0, i64* %9, align 1
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
  %RSP_2 = sub i64 %RSP_1, 16
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 10
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %RIP_5 = add i64 %RIP_4, 2
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %RAX_0 = load i64, i64* %RAX
  %AX_0 = trunc i64 %RAX_0 to i16
  %10 = and i16 %AX_0, -256
  %AX_1 = or i16 0, %10
  %EAX_0 = trunc i64 %RAX_0 to i32
  %11 = and i32 %EAX_0, -256
  %EAX_1 = or i32 0, %11
  %12 = and i64 %RAX_0, -256
  %RAX_1 = or i64 0, %12
  %RIP_6 = add i64 %RIP_5, 5
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RSP_3 = sub i64 %RSP_2, 8
  %13 = inttoptr i64 %RSP_3 to i64*
  store i64 4195945, i64* %13
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  store i8 0, i8* %AL
  store i16 %AX_1, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i16 1818, i16* %DI
  store i8 26, i8* %DIL
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 4196122, i32* %EDI
  %ZF_0 = icmp eq i64 %RSP_2, 0
  %SF_0 = icmp slt i64 %RSP_2, 0
  %14 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP_1, i64 16)
  %OF_0 = extractvalue { i64, i1 } %14, 1
  %15 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP_1, i64 16)
  %CF_0 = extractvalue { i64, i1 } %15, 1
  %16 = trunc i64 %RSP_2 to i8
  %17 = call i8 @llvm.ctpop.i8(i8 %16)
  %18 = trunc i8 %17 to i1
  %PF_0 = icmp eq i1 %18, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %19 = zext i1 %CF_0 to i32
  %20 = shl i32 %19, 0
  %21 = or i32 %20, %CtlSysEFLAGS_0
  %22 = zext i1 %PF_0 to i32
  %23 = shl i32 %22, 2
  %24 = or i32 %23, %21
  %25 = zext i1 false to i32
  %26 = shl i32 %25, 4
  %27 = or i32 %26, %24
  %28 = zext i1 %ZF_0 to i32
  %29 = shl i32 %28, 6
  %30 = or i32 %29, %27
  %31 = zext i1 %SF_0 to i32
  %32 = shl i32 %31, 7
  %33 = or i32 %32, %30
  %34 = zext i1 %OF_0 to i32
  %35 = shl i32 %34, 11
  %EFLAGS_1 = or i32 %35, %33
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_5, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_5, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4196122, i64* %RDI
  store i64 %RIP_6, i64* %RIP
  store i64 %RSP_3, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
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
  %RIP_7 = load i64, i64* %RIP
  %RIP_8 = add i64 %RIP_7, 3
  %EIP_6 = trunc i64 %RIP_8 to i32
  %IP_6 = trunc i64 %RIP_8 to i16
  %RAX_2 = load i64, i64* %RAX
  %EAX_2 = trunc i64 %RAX_2 to i32
  %RBP_1 = load i64, i64* %RBP
  %50 = add i64 %RBP_1, -4
  %51 = inttoptr i64 %50 to i32*
  store i32 %EAX_2, i32* %51, align 1
  %RIP_9 = add i64 %RIP_8, 4
  %EIP_7 = trunc i64 %RIP_9 to i32
  %IP_7 = trunc i64 %RIP_9 to i16
  %RSP_4 = load i64, i64* %RSP
  %RSP_5 = add i64 %RSP_4, 16
  %ESP_3 = trunc i64 %RSP_5 to i32
  %SP_3 = trunc i64 %RSP_5 to i16
  %SPL_3 = trunc i64 %RSP_5 to i8
  %EFLAGS_2 = load i32, i32* %EFLAGS
  %RIP_10 = add i64 %RIP_9, 1
  %EIP_8 = trunc i64 %RIP_10 to i32
  %IP_8 = trunc i64 %RIP_10 to i16
  %RSP_6 = add i64 %RSP_5, 8
  %ESP_4 = trunc i64 %RSP_6 to i32
  %SP_4 = trunc i64 %RSP_6 to i16
  %SPL_4 = trunc i64 %RSP_6 to i8
  %52 = sub i64 %RSP_6, 8
  %53 = inttoptr i64 %52 to i64*
  %RBP_2 = load i64, i64* %53, align 1
  %EBP_1 = trunc i64 %RBP_2 to i32
  %BP_1 = trunc i64 %RBP_2 to i16
  %BPL_1 = trunc i64 %RBP_2 to i8
  %RIP_11 = add i64 %RIP_10, 1
  %EIP_9 = trunc i64 %RIP_11 to i32
  %IP_9 = trunc i64 %RIP_11 to i16
  %RSP_7 = add i64 %RSP_6, 8
  %54 = inttoptr i64 %RSP_6 to i64*
  %RIP_12 = load i64, i64* %54
  %ESP_5 = trunc i64 %RSP_7 to i32
  %SP_5 = trunc i64 %RSP_7 to i16
  %SPL_5 = trunc i64 %RSP_7 to i8
  %EIP_10 = trunc i64 %RIP_12 to i32
  %IP_10 = trunc i64 %RIP_12 to i16
  %ZF_1 = icmp eq i64 %RSP_5, 0
  %SF_1 = icmp slt i64 %RSP_5, 0
  %55 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_4, i64 16)
  %OF_1 = extractvalue { i64, i1 } %55, 1
  %56 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_4, i64 16)
  %CF_1 = extractvalue { i64, i1 } %56, 1
  %57 = trunc i64 %RSP_5 to i8
  %58 = call i8 @llvm.ctpop.i8(i8 %57)
  %59 = trunc i8 %58 to i1
  %PF_1 = icmp eq i1 %59, false
  %60 = zext i1 %CF_1 to i32
  %61 = shl i32 %60, 0
  %62 = or i32 %61, %CtlSysEFLAGS_0
  %63 = zext i1 %PF_1 to i32
  %64 = shl i32 %63, 2
  %65 = or i32 %64, %62
  %66 = zext i1 false to i32
  %67 = shl i32 %66, 4
  %68 = or i32 %67, %65
  %69 = zext i1 %ZF_1 to i32
  %70 = shl i32 %69, 6
  %71 = or i32 %70, %68
  %72 = zext i1 %SF_1 to i32
  %73 = shl i32 %72, 7
  %74 = or i32 %73, %71
  %75 = zext i1 %OF_1 to i32
  %76 = shl i32 %75, 11
  %EFLAGS_3 = or i32 %76, %74
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EAX_2, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 %EFLAGS_3, i32* %EFLAGS
  store i32 %EIP_10, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_10, i16* %IP
  store i64 %RAX_2, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 %RIP_12, i64* %RIP
  store i64 %RSP_7, i64* %RSP
  store i16 %SP_5, i16* %SP
  store i8 %SPL_5, i8* %SPL
  br label %exit_fn_400650
}

define void @fn_4004F0(%regset* noalias nocapture) {
entry_fn_4004F0:
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
  br label %bb_4004F0

exit_fn_4004F0:                                   ; preds = %bb_4004F0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  ret void

bb_4004F0:                                        ; preds = %entry_fn_4004F0
  %RIP_1 = add i64 4195568, 6
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %2 = add i64 %RIP_1, 2100002
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
  br label %exit_fn_4004F0
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
  call void @fn_400600(%regset* %3)
  %6 = call i32 @main_fini_regset(%regset* %3)
  ret i32 %6
}
