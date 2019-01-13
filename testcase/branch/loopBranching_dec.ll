; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

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
    %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
    %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
    %ZMM0 = alloca <16 x float>
    store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
    %1 = bitcast <16 x float> %ZMM0_init to i512
    %2 = trunc i512 %1 to i128
    %XMM0_init = bitcast i128 %2 to <4 x float>
    %XMM0 = alloca <4 x float>
    store <4 x float> %XMM0_init, <4 x float>* %XMM0
    %3 = bitcast <16 x float> %ZMM0_init to i512
    %4 = trunc i512 %3 to i256
    %YMM0_init = bitcast i256 %4 to <8 x float>
    %YMM0 = alloca <8 x float>
    store <8 x float> %YMM0_init, <8 x float>* %YMM0
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
    %5 = lshr i64 %RAX_init, 8
    %AH_init = trunc i64 %5 to i8
    %AH = alloca i8
    store i8 %AH_init, i8* %AH
    %ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
    %ZMM1_init = load <16 x float>, <16 x float>* %ZMM1_ptr
    %ZMM1 = alloca <16 x float>
    store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
    %6 = bitcast <16 x float> %ZMM1_init to i512
    %7 = trunc i512 %6 to i128
    %XMM1_init = bitcast i128 %7 to <4 x float>
    %XMM1 = alloca <4 x float>
    store <4 x float> %XMM1_init, <4 x float>* %XMM1
    %8 = bitcast <16 x float> %ZMM1_init to i512
    %9 = trunc i512 %8 to i256
    %YMM1_init = bitcast i256 %9 to <8 x float>
    %YMM1 = alloca <8 x float>
    store <8 x float> %YMM1_init, <8 x float>* %YMM1
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
    %10 = lshr i64 %RCX_init, 8
    %CH_init = trunc i64 %10 to i8
    %CH = alloca i8
    store i8 %CH_init, i8* %CH
    br label %bb_4004D0

  exit_fn_4004D0:                                   ; preds = %bb_4005EB
    %11 = load i32, i32* %CtlSysEFLAGS
    store i32 %11, i32* %CtlSysEFLAGS_ptr
    %12 = load i32, i32* %EFLAGS
    store i32 %12, i32* %EFLAGS_ptr
    %13 = load i64, i64* %RAX
    store i64 %13, i64* %RAX_ptr
    %14 = load i64, i64* %RBP
    store i64 %14, i64* %RBP_ptr
    %15 = load i64, i64* %RCX
    store i64 %15, i64* %RCX_ptr
    %16 = load i64, i64* %RDI
    store i64 %16, i64* %RDI_ptr
    %17 = load i64, i64* %RIP
    store i64 %17, i64* %RIP_ptr
    %18 = load i64, i64* %RSI
    store i64 %18, i64* %RSI_ptr
    %19 = load i64, i64* %RSP
    store i64 %19, i64* %RSP_ptr
    %20 = load <16 x float>, <16 x float>* %ZMM0
    store <16 x float> %20, <16 x float>* %ZMM0_ptr
    %21 = load <16 x float>, <16 x float>* %ZMM1
    store <16 x float> %21, <16 x float>* %ZMM1_ptr
    ret void

  bb_4004D0:                                        ; preds = %entry_fn_4004D0
    %RIP_1 = add i64 4195536, 1
    %EIP_0 = trunc i64 %RIP_1 to i32
    %IP_0 = trunc i64 %RIP_1 to i16
    %RBP_0 = load i64, i64* %RBP
    %RSP_0 = load i64, i64* %RSP
    %22 = sub i64 %RSP_0, 8
    %23 = inttoptr i64 %22 to i64*
    store i64 %RBP_0, i64* %23, align 1
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
    %RSP_2 = sub i64 %RSP_1, 48
    %ESP_1 = trunc i64 %RSP_2 to i32
    %SP_1 = trunc i64 %RSP_2 to i16
    %SPL_1 = trunc i64 %RSP_2 to i8
    %EFLAGS_0 = load i32, i32* %EFLAGS
    %RIP_4 = add i64 %RIP_3, 10
    %EIP_3 = trunc i64 %RIP_4 to i32
    %IP_3 = trunc i64 %RIP_4 to i16
    %RIP_5 = add i64 %RIP_4, 8
    %EIP_4 = trunc i64 %RIP_5 to i32
    %IP_4 = trunc i64 %RIP_5 to i16
    %24 = add i64 %RIP_5, 430
    %25 = inttoptr i64 %24 to double*
    %26 = load double, double* %25, align 1
    %27 = bitcast double %26 to i64
    %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
    %28 = bitcast <16 x float> %ZMM0_0 to i512
    %XMM0_0 = trunc i512 %28 to i128
    %29 = zext i64 %27 to i128
    %30 = and i128 %XMM0_0, -18446744073709551616
    %XMM0_1 = or i128 %29, %30
    %31 = bitcast <16 x float> %ZMM0_0 to i512
    %YMM0_0 = trunc i512 %31 to i256
    %32 = zext i128 %XMM0_1 to i256
    %33 = and i256 %YMM0_0, -340282366920938463463374607431768211456
    %YMM0_1 = or i256 %32, %33
    %34 = bitcast <16 x float> %ZMM0_0 to i512
    %35 = zext i128 %XMM0_1 to i512
    %36 = and i512 %34, -340282366920938463463374607431768211456
    %ZMM0_1 = or i512 %35, %36
    %RIP_6 = add i64 %RIP_5, 7
    %EIP_5 = trunc i64 %RIP_6 to i32
    %IP_5 = trunc i64 %RIP_6 to i16
    %37 = add i64 %RSP_1, -4
    %38 = inttoptr i64 %37 to i32*
    store i32 0, i32* %38, align 1
    %RIP_7 = add i64 %RIP_6, 7
    %EIP_6 = trunc i64 %RIP_7 to i32
    %IP_6 = trunc i64 %RIP_7 to i16
    %39 = add i64 %RSP_1, -8
    %40 = inttoptr i64 %39 to i32*
    store i32 5, i32* %40, align 1
    %RIP_8 = add i64 %RIP_7, 5
    %EIP_7 = trunc i64 %RIP_8 to i32
    %IP_7 = trunc i64 %RIP_8 to i16
    %41 = trunc i128 %XMM0_1 to i64
    %42 = bitcast i64 %41 to double
    %43 = add i64 %RSP_1, -16
    %44 = inttoptr i64 %43 to double*
    store double %42, double* %44, align 1
    %RIP_9 = add i64 %RIP_8, 7
    %EIP_8 = trunc i64 %RIP_9 to i32
    %IP_8 = trunc i64 %RIP_9 to i16
    %45 = add i64 %RSP_1, -20
    %46 = inttoptr i64 %45 to i32*
    store i32 100, i32* %46, align 1
    %RIP_10 = add i64 %RIP_9, 3
    %EIP_9 = trunc i64 %RIP_10 to i32
    %IP_9 = trunc i64 %RIP_10 to i16
    %47 = add i64 %RSP_1, -20
    %48 = inttoptr i64 %47 to i32*
    %ESI_0 = load i32, i32* %48, align 1
    %RSI_0 = load i64, i64* %RSI
    %RSI_1 = zext i32 %ESI_0 to i64
    %SI_0 = trunc i32 %ESI_0 to i16
    %SIL_0 = trunc i32 %ESI_0 to i8
    %RIP_11 = add i64 %RIP_10, 5
    %EIP_10 = trunc i64 %RIP_11 to i32
    %IP_10 = trunc i64 %RIP_11 to i16
    %49 = add i64 %RSP_1, -16
    %50 = inttoptr i64 %49 to double*
    %51 = load double, double* %50, align 1
    %52 = bitcast double %51 to i64
    %53 = zext i64 %52 to i128
    %54 = and i128 %XMM0_1, -18446744073709551616
    %XMM0_2 = or i128 %53, %54
    %55 = zext i128 %XMM0_2 to i256
    %56 = and i256 %YMM0_1, -340282366920938463463374607431768211456
    %YMM0_2 = or i256 %55, %56
    %57 = zext i128 %XMM0_2 to i512
    %58 = and i512 %ZMM0_1, -340282366920938463463374607431768211456
    %ZMM0_2 = or i512 %57, %58
    %RIP_12 = add i64 %RIP_11, 2
    %EIP_11 = trunc i64 %RIP_12 to i32
    %IP_11 = trunc i64 %RIP_12 to i16
    %RAX_0 = load i64, i64* %RAX
    %AX_0 = trunc i64 %RAX_0 to i16
    %59 = and i16 %AX_0, -256
    %AX_1 = or i16 1, %59
    %EAX_0 = trunc i64 %RAX_0 to i32
    %60 = and i32 %EAX_0, -256
    %EAX_1 = or i32 1, %60
    %61 = and i64 %RAX_0, -256
    %RAX_1 = or i64 1, %61
    %RIP_13 = add i64 %RIP_12, 5
    %EIP_12 = trunc i64 %RIP_13 to i32
    %IP_12 = trunc i64 %RIP_13 to i16
    %RSP_3 = sub i64 %RSP_2, 8
    %62 = inttoptr i64 %RSP_3 to i64*
    store i64 4195603, i64* %62
    %ESP_2 = trunc i64 %RSP_3 to i32
    %SP_2 = trunc i64 %RSP_3 to i16
    %SPL_2 = trunc i64 %RSP_3 to i8
    store i8 1, i8* %AL
    store i16 %AX_1, i16* %AX
    store i16 %BP_0, i16* %BP
    store i8 %BPL_0, i8* %BPL
    store i16 1696, i16* %DI
    store i8 -96, i8* %DIL
    store i32 %EAX_1, i32* %EAX
    store i32 %EBP_0, i32* %EBP
    store i32 4196000, i32* %EDI
    %ZF_0 = icmp eq i64 %RSP_2, 0
    %SF_0 = icmp slt i64 %RSP_2, 0
    %63 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP_1, i64 48)
    %OF_0 = extractvalue { i64, i1 } %63, 1
    %64 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP_1, i64 48)
    %CF_0 = extractvalue { i64, i1 } %64, 1
    %65 = trunc i64 %RSP_2 to i8
    %66 = call i8 @llvm.ctpop.i8(i8 %65)
    %67 = trunc i8 %66 to i1
    %PF_0 = icmp eq i1 %67, false
    %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
    %68 = zext i1 %CF_0 to i32
    %69 = shl i32 %68, 0
    %70 = or i32 %69, %CtlSysEFLAGS_0
    %71 = zext i1 %PF_0 to i32
    %72 = shl i32 %71, 2
    %73 = or i32 %72, %70
    %74 = zext i1 false to i32
    %75 = shl i32 %74, 4
    %76 = or i32 %75, %73
    %77 = zext i1 %ZF_0 to i32
    %78 = shl i32 %77, 6
    %79 = or i32 %78, %76
    %80 = zext i1 %SF_0 to i32
    %81 = shl i32 %80, 7
    %82 = or i32 %81, %79
    %83 = zext i1 %OF_0 to i32
    %84 = shl i32 %83, 11
    %EFLAGS_1 = or i32 %84, %82
    store i32 %EFLAGS_1, i32* %EFLAGS
    store i32 %EIP_12, i32* %EIP
    store i32 %ESI_0, i32* %ESI
    store i32 %ESP_2, i32* %ESP
    store i16 %IP_12, i16* %IP
    store i64 %RAX_1, i64* %RAX
    store i64 %RSP_1, i64* %RBP
    store i64 4196000, i64* %RDI
    store i64 %RIP_13, i64* %RIP
    store i64 %RSI_1, i64* %RSI
    store i64 %RSP_3, i64* %RSP
    store i16 %SI_0, i16* %SI
    store i8 %SIL_0, i8* %SIL
    store i16 %SP_2, i16* %SP
    store i8 %SPL_2, i8* %SPL
    %85 = bitcast i128 %XMM0_2 to <4 x float>
    store <4 x float> %85, <4 x float>* %XMM0
    %86 = bitcast i256 %YMM0_2 to <8 x float>
    store <8 x float> %86, <8 x float>* %YMM0
    %87 = bitcast i512 %ZMM0_2 to <16 x float>
    store <16 x float> %87, <16 x float>* %ZMM0
    %88 = load i32, i32* %CtlSysEFLAGS
    store i32 %88, i32* %CtlSysEFLAGS_ptr
    %89 = load i32, i32* %EFLAGS
    store i32 %89, i32* %EFLAGS_ptr
    %90 = load i64, i64* %RAX
    store i64 %90, i64* %RAX_ptr
    %91 = load i64, i64* %RBP
    store i64 %91, i64* %RBP_ptr
    %92 = load i64, i64* %RCX
    store i64 %92, i64* %RCX_ptr
    %93 = load i64, i64* %RDI
    store i64 %93, i64* %RDI_ptr
    %94 = load i64, i64* %RIP
    store i64 %94, i64* %RIP_ptr
    %95 = load i64, i64* %RSI
    store i64 %95, i64* %RSI_ptr
    %96 = load i64, i64* %RSP
    store i64 %96, i64* %RSP_ptr
    %97 = load <16 x float>, <16 x float>* %ZMM0
    store <16 x float> %97, <16 x float>* %ZMM0_ptr
    %98 = load <16 x float>, <16 x float>* %ZMM1
    store <16 x float> %98, <16 x float>* %ZMM1_ptr
    call void @fn_4003D0(%regset* %0)
    %99 = load i32, i32* %CtlSysEFLAGS_ptr
    store i32 %99, i32* %CtlSysEFLAGS
    %100 = load i32, i32* %EFLAGS_ptr
    store i32 %100, i32* %EFLAGS
    %101 = load i64, i64* %RAX_ptr
    store i64 %101, i64* %RAX
    %102 = load i64, i64* %RBP_ptr
    store i64 %102, i64* %RBP
    %103 = load i64, i64* %RCX_ptr
    store i64 %103, i64* %RCX
    %104 = load i64, i64* %RDI_ptr
    store i64 %104, i64* %RDI
    %105 = load i64, i64* %RIP_ptr
    store i64 %105, i64* %RIP
    %106 = load i64, i64* %RSI_ptr
    store i64 %106, i64* %RSI
    %107 = load i64, i64* %RSP_ptr
    store i64 %107, i64* %RSP
    %108 = load <16 x float>, <16 x float>* %ZMM0_ptr
    store <16 x float> %108, <16 x float>* %ZMM0
    %109 = load <16 x float>, <16 x float>* %ZMM1_ptr
    store <16 x float> %109, <16 x float>* %ZMM1
    %RIP_14 = load i64, i64* %RIP
    %RIP_15 = add i64 %RIP_14, 7
    %EIP_13 = trunc i64 %RIP_15 to i32
    %IP_13 = trunc i64 %RIP_15 to i16
    %RBP_1 = load i64, i64* %RBP
    %110 = add i64 %RBP_1, -24
    %111 = inttoptr i64 %110 to i32*
    store i32 0, i32* %111, align 1
    %RIP_16 = add i64 %RIP_15, 3
    %EIP_14 = trunc i64 %RIP_16 to i32
    %IP_14 = trunc i64 %RIP_16 to i16
    %RAX_2 = load i64, i64* %RAX
    %EAX_2 = trunc i64 %RAX_2 to i32
    %112 = add i64 %RBP_1, -36
    %113 = inttoptr i64 %112 to i32*
    store i32 %EAX_2, i32* %113, align 1
    store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
    store i32 %EAX_2, i32* %EAX
    store i32 %EIP_14, i32* %EIP
    store i16 %IP_14, i16* %IP
    store i64 %RAX_2, i64* %RAX
    store i64 %RBP_1, i64* %RBP
    store i64 %RIP_16, i64* %RIP
    br label %bb_40051D

  bb_40051D:                                        ; preds = %bb_4005DD, %bb_4004D0
    %RIP_85 = add i64 4195613, 3
    %EIP_73 = trunc i64 %RIP_85 to i32
    %IP_73 = trunc i64 %RIP_85 to i16
    %RBP_10 = load i64, i64* %RBP
    %114 = add i64 %RBP_10, -24
    %115 = inttoptr i64 %114 to i32*
    %EAX_18 = load i32, i32* %115, align 1
    %RAX_22 = load i64, i64* %RAX
    %RAX_23 = zext i32 %EAX_18 to i64
    %AX_15 = trunc i32 %EAX_18 to i16
    %AL_12 = trunc i32 %EAX_18 to i8
    %116 = lshr i32 %EAX_18, 8
    %AH_8 = trunc i32 %116 to i8
    %RIP_86 = add i64 %RIP_85, 3
    %EIP_74 = trunc i64 %RIP_86 to i32
    %IP_74 = trunc i64 %RIP_86 to i16
    %117 = add i64 %RBP_10, -8
    %118 = inttoptr i64 %117 to i32*
    %119 = load i32, i32* %118, align 1
    %CC_A_0 = icmp ugt i32 %EAX_18, %119
    %CC_AE_0 = icmp uge i32 %EAX_18, %119
    %CC_B_0 = icmp ult i32 %EAX_18, %119
    %CC_BE_014 = icmp ule i32 %EAX_18, %119
    %CC_L_0 = icmp slt i32 %EAX_18, %119
    %CC_LE_0 = icmp sle i32 %EAX_18, %119
    %CC_G_0 = icmp sgt i32 %EAX_18, %119
    %CC_GE_0 = icmp sge i32 %EAX_18, %119
    %CC_E_0 = icmp eq i32 %EAX_18, %119
    %CC_NE_0 = icmp ne i32 %EAX_18, %119
    %120 = sub i32 %EAX_18, %119
    %ZF_015 = icmp eq i32 %120, 0
    %SF_016 = icmp slt i32 %120, 0
    %121 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_18, i32 %119)
    %OF_017 = extractvalue { i32, i1 } %121, 1
    %122 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_18, i32 %119)
    %CF_018 = extractvalue { i32, i1 } %122, 1
    %123 = trunc i32 %120 to i8
    %124 = call i8 @llvm.ctpop.i8(i8 %123)
    %125 = trunc i8 %124 to i1
    %PF_019 = icmp eq i1 %125, false
    %CtlSysEFLAGS_4 = load i32, i32* %CtlSysEFLAGS
    %126 = zext i1 %CF_018 to i32
    %127 = shl i32 %126, 0
    %128 = or i32 %127, %CtlSysEFLAGS_4
    %129 = zext i1 %PF_019 to i32
    %130 = shl i32 %129, 2
    %131 = or i32 %130, %128
    %132 = zext i1 false to i32
    %133 = shl i32 %132, 4
    %134 = or i32 %133, %131
    %135 = zext i1 %ZF_015 to i32
    %136 = shl i32 %135, 6
    %137 = or i32 %136, %134
    %138 = zext i1 %SF_016 to i32
    %139 = shl i32 %138, 7
    %140 = or i32 %139, %137
    %141 = zext i1 %OF_017 to i32
    %142 = shl i32 %141, 11
    %EFLAGS_7 = or i32 %142, %140
    %RIP_87 = add i64 %RIP_86, 6
    %EIP_75 = trunc i64 %RIP_87 to i32
    %IP_75 = trunc i64 %RIP_87 to i16
    store i8 %AH_8, i8* %AH
    store i8 %AL_12, i8* %AL
    store i16 %AX_15, i16* %AX
    store i32 %CtlSysEFLAGS_4, i32* %CtlSysEFLAGS
    store i32 %EAX_18, i32* %EAX
    store i32 %EFLAGS_7, i32* %EFLAGS
    store i32 4195819, i32* %EIP
    store i16 1515, i16* %IP
    store i64 %RAX_23, i64* %RAX
    store i64 %RBP_10, i64* %RBP
    store i64 4195819, i64* %RIP
    br i1 %CC_GE_0, label %bb_4005EB, label %bb_400529

  bb_400529:                                        ; preds = %bb_40051D
    %RIP_18 = add i64 4195625, 5
    %EIP_15 = trunc i64 %RIP_18 to i32
    %IP_15 = trunc i64 %RIP_18 to i16
    %RBP_2 = load i64, i64* %RBP
    %143 = add i64 %RBP_2, -16
    %144 = inttoptr i64 %143 to double*
    %145 = load double, double* %144, align 1
    %146 = bitcast double %145 to i64
    %ZMM0_3 = load <16 x float>, <16 x float>* %ZMM0
    %147 = bitcast <16 x float> %ZMM0_3 to i512
    %XMM0_3 = trunc i512 %147 to i128
    %148 = zext i64 %146 to i128
    %149 = and i128 %XMM0_3, -18446744073709551616
    %XMM0_4 = or i128 %148, %149
    %150 = bitcast <16 x float> %ZMM0_3 to i512
    %YMM0_3 = trunc i512 %150 to i256
    %151 = zext i128 %XMM0_4 to i256
    %152 = and i256 %YMM0_3, -340282366920938463463374607431768211456
    %YMM0_4 = or i256 %151, %152
    %153 = bitcast <16 x float> %ZMM0_3 to i512
    %154 = zext i128 %XMM0_4 to i512
    %155 = and i512 %153, -340282366920938463463374607431768211456
    %ZMM0_4 = or i512 %154, %155
    %RIP_19 = add i64 %RIP_18, 3
    %EIP_16 = trunc i64 %RIP_19 to i32
    %IP_16 = trunc i64 %RIP_19 to i16
    %156 = add i64 %RBP_2, -8
    %157 = inttoptr i64 %156 to i32*
    %EAX_3 = load i32, i32* %157, align 1
    %RAX_3 = load i64, i64* %RAX
    %RAX_4 = zext i32 %EAX_3 to i64
    %AX_2 = trunc i32 %EAX_3 to i16
    %AL_1 = trunc i32 %EAX_3 to i8
    %158 = lshr i32 %EAX_3, 8
    %AH_0 = trunc i32 %158 to i8
    %RIP_20 = add i64 %RIP_19, 4
    %EIP_17 = trunc i64 %RIP_20 to i32
    %IP_17 = trunc i64 %RIP_20 to i16
    %159 = sitofp i32 %EAX_3 to double
    %160 = bitcast double %159 to i64
    %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
    %161 = bitcast <16 x float> %ZMM1_0 to i512
    %XMM1_0 = trunc i512 %161 to i128
    %162 = zext i64 %160 to i128
    %163 = and i128 %XMM1_0, -18446744073709551616
    %XMM1_1 = or i128 %162, %163
    %164 = bitcast <16 x float> %ZMM1_0 to i512
    %YMM1_0 = trunc i512 %164 to i256
    %165 = zext i128 %XMM1_1 to i256
    %166 = and i256 %YMM1_0, -340282366920938463463374607431768211456
    %YMM1_1 = or i256 %165, %166
    %167 = bitcast <16 x float> %ZMM1_0 to i512
    %168 = zext i128 %XMM1_1 to i512
    %169 = and i512 %167, -340282366920938463463374607431768211456
    %ZMM1_1 = or i512 %168, %169
    %RIP_21 = add i64 %RIP_20, 4
    %EIP_18 = trunc i64 %RIP_21 to i32
    %IP_18 = trunc i64 %RIP_21 to i16
    %170 = trunc i128 %XMM0_4 to i64
    %171 = bitcast i64 %170 to double
    %172 = trunc i128 %XMM1_1 to i64
    %173 = bitcast i64 %172 to double
    %174 = fdiv double %171, %173
    %175 = bitcast double %174 to i64
    %176 = zext i64 %175 to i128
    %177 = and i128 %XMM0_4, -18446744073709551616
    %XMM0_5 = or i128 %176, %177
    %178 = zext i128 %XMM0_5 to i256
    %179 = and i256 %YMM0_4, -340282366920938463463374607431768211456
    %YMM0_5 = or i256 %178, %179
    %180 = zext i128 %XMM0_5 to i512
    %181 = and i512 %ZMM0_4, -340282366920938463463374607431768211456
    %ZMM0_5 = or i512 %180, %181
    %RIP_22 = add i64 %RIP_21, 5
    %EIP_19 = trunc i64 %RIP_22 to i32
    %IP_19 = trunc i64 %RIP_22 to i16
    %182 = trunc i128 %XMM0_5 to i64
    %183 = bitcast i64 %182 to double
    %184 = add i64 %RBP_2, -32
    %185 = inttoptr i64 %184 to double*
    store double %183, double* %185, align 1
    %RIP_23 = add i64 %RIP_22, 5
    %EIP_20 = trunc i64 %RIP_23 to i32
    %IP_20 = trunc i64 %RIP_23 to i16
    %186 = add i64 %RBP_2, -32
    %187 = inttoptr i64 %186 to double*
    %188 = load double, double* %187, align 1
    %189 = bitcast double %188 to i64
    %190 = zext i64 %189 to i128
    %191 = and i128 %XMM0_5, -18446744073709551616
    %XMM0_6 = or i128 %190, %191
    %192 = zext i128 %XMM0_6 to i256
    %193 = and i256 %YMM0_5, -340282366920938463463374607431768211456
    %YMM0_6 = or i256 %192, %193
    %194 = zext i128 %XMM0_6 to i512
    %195 = and i512 %ZMM0_5, -340282366920938463463374607431768211456
    %ZMM0_6 = or i512 %194, %195
    %RIP_24 = add i64 %RIP_23, 3
    %EIP_21 = trunc i64 %RIP_24 to i32
    %IP_21 = trunc i64 %RIP_24 to i16
    %196 = add i64 %RBP_2, -20
    %197 = inttoptr i64 %196 to i32*
    %EAX_4 = load i32, i32* %197, align 1
    %RAX_5 = zext i32 %EAX_4 to i64
    %AX_3 = trunc i32 %EAX_4 to i16
    %AL_2 = trunc i32 %EAX_4 to i8
    %198 = lshr i32 %EAX_4, 8
    %AH_1 = trunc i32 %198 to i8
    %RIP_25 = add i64 %RIP_24, 4
    %EIP_22 = trunc i64 %RIP_25 to i32
    %IP_22 = trunc i64 %RIP_25 to i16
    %199 = sitofp i32 %EAX_4 to double
    %200 = bitcast double %199 to i64
    %201 = zext i64 %200 to i128
    %202 = and i128 %XMM1_1, -18446744073709551616
    %XMM1_2 = or i128 %201, %202
    %203 = zext i128 %XMM1_2 to i256
    %204 = and i256 %YMM1_1, -340282366920938463463374607431768211456
    %YMM1_2 = or i256 %203, %204
    %205 = zext i128 %XMM1_2 to i512
    %206 = and i512 %ZMM1_1, -340282366920938463463374607431768211456
    %ZMM1_2 = or i512 %205, %206
    %RIP_26 = add i64 %RIP_25, 4
    %EIP_23 = trunc i64 %RIP_26 to i32
    %IP_23 = trunc i64 %RIP_26 to i16
    %207 = trunc i128 %XMM1_2 to i64
    %208 = bitcast i64 %207 to double
    %209 = trunc i128 %XMM0_6 to i64
    %210 = bitcast i64 %209 to double
    %ZF_01 = fcmp ueq double %208, %210
    %PF_02 = fcmp uno double %208, %210
    %CF_03 = fcmp ult double %208, %210
    %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
    %211 = zext i1 %CF_03 to i32
    %212 = shl i32 %211, 0
    %213 = or i32 %212, %CtlSysEFLAGS_1
    %214 = zext i1 %PF_02 to i32
    %215 = shl i32 %214, 2
    %216 = or i32 %215, %213
    %217 = zext i1 false to i32
    %218 = shl i32 %217, 4
    %219 = or i32 %218, %216
    %220 = zext i1 %ZF_01 to i32
    %221 = shl i32 %220, 6
    %222 = or i32 %221, %219
    %223 = zext i1 false to i32
    %224 = shl i32 %223, 7
    %225 = or i32 %224, %222
    %226 = zext i1 false to i32
    %227 = shl i32 %226, 11
    %EFLAGS_2 = or i32 %227, %225
    %RIP_27 = add i64 %RIP_26, 6
    %EIP_24 = trunc i64 %RIP_27 to i32
    %IP_24 = trunc i64 %RIP_27 to i16
    %CC_BE_0 = or i1 %CF_03, %ZF_01
    store i8 %AH_1, i8* %AH
    store i8 %AL_2, i8* %AL
    store i16 %AX_3, i16* %AX
    store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
    store i32 %EAX_4, i32* %EAX
    store i32 %EFLAGS_2, i32* %EFLAGS
    store i32 4195721, i32* %EIP
    store i16 1417, i16* %IP
    store i64 %RAX_5, i64* %RAX
    store i64 %RBP_2, i64* %RBP
    store i64 4195721, i64* %RIP
    %228 = bitcast i128 %XMM0_6 to <4 x float>
    store <4 x float> %228, <4 x float>* %XMM0
    %229 = bitcast i128 %XMM1_2 to <4 x float>
    store <4 x float> %229, <4 x float>* %XMM1
    %230 = bitcast i256 %YMM0_6 to <8 x float>
    store <8 x float> %230, <8 x float>* %YMM0
    %231 = bitcast i256 %YMM1_2 to <8 x float>
    store <8 x float> %231, <8 x float>* %YMM1
    %232 = bitcast i512 %ZMM0_6 to <16 x float>
    store <16 x float> %232, <16 x float>* %ZMM0
    %233 = bitcast i512 %ZMM1_2 to <16 x float>
    store <16 x float> %233, <16 x float>* %ZMM1
    br i1 %CC_BE_0, label %bb_400589, label %bb_400554

  bb_400554:                                        ; preds = %bb_400529
    %RIP_42 = add i64 4195668, 10
    %EIP_36 = trunc i64 %RIP_42 to i32
    %IP_36 = trunc i64 %RIP_42 to i16
    %RIP_43 = add i64 %RIP_42, 5
    %EIP_37 = trunc i64 %RIP_43 to i32
    %IP_37 = trunc i64 %RIP_43 to i16
    %RBP_5 = load i64, i64* %RBP
    %234 = add i64 %RBP_5, -32
    %235 = inttoptr i64 %234 to double*
    %236 = load double, double* %235, align 1
    %237 = bitcast double %236 to i64
    %ZMM0_7 = load <16 x float>, <16 x float>* %ZMM0
    %238 = bitcast <16 x float> %ZMM0_7 to i512
    %XMM0_7 = trunc i512 %238 to i128
    %239 = zext i64 %237 to i128
    %240 = and i128 %XMM0_7, -18446744073709551616
    %XMM0_8 = or i128 %239, %240
    %241 = bitcast <16 x float> %ZMM0_7 to i512
    %YMM0_7 = trunc i512 %241 to i256
    %242 = zext i128 %XMM0_8 to i256
    %243 = and i256 %YMM0_7, -340282366920938463463374607431768211456
    %YMM0_8 = or i256 %242, %243
    %244 = bitcast <16 x float> %ZMM0_7 to i512
    %245 = zext i128 %XMM0_8 to i512
    %246 = and i512 %244, -340282366920938463463374607431768211456
    %ZMM0_8 = or i512 %245, %246
    %RIP_44 = add i64 %RIP_43, 5
    %EIP_38 = trunc i64 %RIP_44 to i32
    %IP_38 = trunc i64 %RIP_44 to i16
    %247 = trunc i128 %XMM0_8 to i64
    %248 = bitcast i64 %247 to double
    %249 = add i64 %RBP_5, -16
    %250 = inttoptr i64 %249 to double*
    %251 = load double, double* %250, align 1
    %252 = fadd double %248, %251
    %253 = bitcast double %252 to i64
    %254 = zext i64 %253 to i128
    %255 = and i128 %XMM0_8, -18446744073709551616
    %XMM0_9 = or i128 %254, %255
    %256 = zext i128 %XMM0_9 to i256
    %257 = and i256 %YMM0_8, -340282366920938463463374607431768211456
    %YMM0_9 = or i256 %256, %257
    %258 = zext i128 %XMM0_9 to i512
    %259 = and i512 %ZMM0_8, -340282366920938463463374607431768211456
    %ZMM0_9 = or i512 %258, %259
    %RIP_45 = add i64 %RIP_44, 5
    %EIP_39 = trunc i64 %RIP_45 to i32
    %IP_39 = trunc i64 %RIP_45 to i16
    %260 = trunc i128 %XMM0_9 to i64
    %261 = bitcast i64 %260 to double
    %262 = add i64 %RBP_5, -16
    %263 = inttoptr i64 %262 to double*
    store double %261, double* %263, align 1
    %RIP_46 = add i64 %RIP_45, 5
    %EIP_40 = trunc i64 %RIP_46 to i32
    %IP_40 = trunc i64 %RIP_46 to i16
    %264 = add i64 %RBP_5, -32
    %265 = inttoptr i64 %264 to double*
    %266 = load double, double* %265, align 1
    %267 = bitcast double %266 to i64
    %268 = zext i64 %267 to i128
    %269 = and i128 %XMM0_9, -18446744073709551616
    %XMM0_10 = or i128 %268, %269
    %270 = zext i128 %XMM0_10 to i256
    %271 = and i256 %YMM0_9, -340282366920938463463374607431768211456
    %YMM0_10 = or i256 %270, %271
    %272 = zext i128 %XMM0_10 to i512
    %273 = and i512 %ZMM0_9, -340282366920938463463374607431768211456
    %ZMM0_10 = or i512 %272, %273
    %RIP_47 = add i64 %RIP_46, 3
    %EIP_41 = trunc i64 %RIP_47 to i32
    %IP_41 = trunc i64 %RIP_47 to i16
    %274 = add i64 %RBP_5, -20
    %275 = inttoptr i64 %274 to i32*
    %ESI_1 = load i32, i32* %275, align 1
    %RSI_2 = load i64, i64* %RSI
    %RSI_3 = zext i32 %ESI_1 to i64
    %SI_1 = trunc i32 %ESI_1 to i16
    %SIL_1 = trunc i32 %ESI_1 to i8
    %RIP_48 = add i64 %RIP_47, 5
    %EIP_42 = trunc i64 %RIP_48 to i32
    %IP_42 = trunc i64 %RIP_48 to i16
    %276 = add i64 %RBP_5, -16
    %277 = inttoptr i64 %276 to double*
    %278 = load double, double* %277, align 1
    %279 = bitcast double %278 to i64
    %ZMM1_3 = load <16 x float>, <16 x float>* %ZMM1
    %280 = bitcast <16 x float> %ZMM1_3 to i512
    %XMM1_3 = trunc i512 %280 to i128
    %281 = zext i64 %279 to i128
    %282 = and i128 %XMM1_3, -18446744073709551616
    %XMM1_4 = or i128 %281, %282
    %283 = bitcast <16 x float> %ZMM1_3 to i512
    %YMM1_3 = trunc i512 %283 to i256
    %284 = zext i128 %XMM1_4 to i256
    %285 = and i256 %YMM1_3, -340282366920938463463374607431768211456
    %YMM1_4 = or i256 %284, %285
    %286 = bitcast <16 x float> %ZMM1_3 to i512
    %287 = zext i128 %XMM1_4 to i512
    %288 = and i512 %286, -340282366920938463463374607431768211456
    %ZMM1_4 = or i512 %287, %288
    %RIP_49 = add i64 %RIP_48, 2
    %EIP_43 = trunc i64 %RIP_49 to i32
    %IP_43 = trunc i64 %RIP_49 to i16
    %RAX_10 = load i64, i64* %RAX
    %AX_7 = trunc i64 %RAX_10 to i16
    %289 = and i16 %AX_7, -256
    %AX_8 = or i16 2, %289
    %EAX_8 = trunc i64 %RAX_10 to i32
    %290 = and i32 %EAX_8, -256
    %EAX_9 = or i32 2, %290
    %291 = and i64 %RAX_10, -256
    %RAX_11 = or i64 2, %291
    %RIP_50 = add i64 %RIP_49, 5
    %EIP_44 = trunc i64 %RIP_50 to i32
    %IP_44 = trunc i64 %RIP_50 to i16
    %RSP_10 = load i64, i64* %RSP
    %RSP_11 = sub i64 %RSP_10, 8
    %292 = inttoptr i64 %RSP_11 to i64*
    store i64 4195713, i64* %292
    %ESP_7 = trunc i64 %RSP_11 to i32
    %SP_7 = trunc i64 %RSP_11 to i16
    %SPL_7 = trunc i64 %RSP_11 to i8
    store i8 2, i8* %AL
    store i16 %AX_8, i16* %AX
    store i16 1723, i16* %DI
    store i8 -69, i8* %DIL
    store i32 %EAX_9, i32* %EAX
    store i32 4196027, i32* %EDI
    store i32 %EIP_44, i32* %EIP
    store i32 %ESI_1, i32* %ESI
    store i32 %ESP_7, i32* %ESP
    store i16 %IP_44, i16* %IP
    store i64 %RAX_11, i64* %RAX
    store i64 %RBP_5, i64* %RBP
    store i64 4196027, i64* %RDI
    store i64 %RIP_50, i64* %RIP
    store i64 %RSI_3, i64* %RSI
    store i64 %RSP_11, i64* %RSP
    store i16 %SI_1, i16* %SI
    store i8 %SIL_1, i8* %SIL
    store i16 %SP_7, i16* %SP
    store i8 %SPL_7, i8* %SPL
    %293 = bitcast i128 %XMM0_10 to <4 x float>
    store <4 x float> %293, <4 x float>* %XMM0
    %294 = bitcast i128 %XMM1_4 to <4 x float>
    store <4 x float> %294, <4 x float>* %XMM1
    %295 = bitcast i256 %YMM0_10 to <8 x float>
    store <8 x float> %295, <8 x float>* %YMM0
    %296 = bitcast i256 %YMM1_4 to <8 x float>
    store <8 x float> %296, <8 x float>* %YMM1
    %297 = bitcast i512 %ZMM0_10 to <16 x float>
    store <16 x float> %297, <16 x float>* %ZMM0
    %298 = bitcast i512 %ZMM1_4 to <16 x float>
    store <16 x float> %298, <16 x float>* %ZMM1
    %299 = load i32, i32* %CtlSysEFLAGS
    store i32 %299, i32* %CtlSysEFLAGS_ptr
    %300 = load i32, i32* %EFLAGS
    store i32 %300, i32* %EFLAGS_ptr
    %301 = load i64, i64* %RAX
    store i64 %301, i64* %RAX_ptr
    %302 = load i64, i64* %RBP
    store i64 %302, i64* %RBP_ptr
    %303 = load i64, i64* %RCX
    store i64 %303, i64* %RCX_ptr
    %304 = load i64, i64* %RDI
    store i64 %304, i64* %RDI_ptr
    %305 = load i64, i64* %RIP
    store i64 %305, i64* %RIP_ptr
    %306 = load i64, i64* %RSI
    store i64 %306, i64* %RSI_ptr
    %307 = load i64, i64* %RSP
    store i64 %307, i64* %RSP_ptr
    %308 = load <16 x float>, <16 x float>* %ZMM0
    store <16 x float> %308, <16 x float>* %ZMM0_ptr
    %309 = load <16 x float>, <16 x float>* %ZMM1
    store <16 x float> %309, <16 x float>* %ZMM1_ptr
    call void @fn_4003D0(%regset* %0)
    %310 = load i32, i32* %CtlSysEFLAGS_ptr
    store i32 %310, i32* %CtlSysEFLAGS
    %311 = load i32, i32* %EFLAGS_ptr
    store i32 %311, i32* %EFLAGS
    %312 = load i64, i64* %RAX_ptr
    store i64 %312, i64* %RAX
    %313 = load i64, i64* %RBP_ptr
    store i64 %313, i64* %RBP
    %314 = load i64, i64* %RCX_ptr
    store i64 %314, i64* %RCX
    %315 = load i64, i64* %RDI_ptr
    store i64 %315, i64* %RDI
    %316 = load i64, i64* %RIP_ptr
    store i64 %316, i64* %RIP
    %317 = load i64, i64* %RSI_ptr
    store i64 %317, i64* %RSI
    %318 = load i64, i64* %RSP_ptr
    store i64 %318, i64* %RSP
    %319 = load <16 x float>, <16 x float>* %ZMM0_ptr
    store <16 x float> %319, <16 x float>* %ZMM0
    %320 = load <16 x float>, <16 x float>* %ZMM1_ptr
    store <16 x float> %320, <16 x float>* %ZMM1
    %RIP_51 = load i64, i64* %RIP
    %RIP_52 = add i64 %RIP_51, 3
    %EIP_45 = trunc i64 %RIP_52 to i32
    %IP_45 = trunc i64 %RIP_52 to i16
    %RAX_12 = load i64, i64* %RAX
    %EAX_10 = trunc i64 %RAX_12 to i32
    %RBP_6 = load i64, i64* %RBP
    %321 = add i64 %RBP_6, -40
    %322 = inttoptr i64 %321 to i32*
    store i32 %EAX_10, i32* %322, align 1
    %RIP_53 = add i64 %RIP_52, 5
    %EIP_46 = trunc i64 %RIP_53 to i32
    %IP_46 = trunc i64 %RIP_53 to i16
    store i32 %EAX_10, i32* %EAX
    store i32 4195800, i32* %EIP
    store i16 1496, i16* %IP
    store i64 %RAX_12, i64* %RAX
    store i64 %RBP_6, i64* %RBP
    store i64 4195800, i64* %RIP
    br label %bb_4005D8

  bb_400589:                                        ; preds = %bb_400529
    %RIP_56 = add i64 4195721, 10
    %EIP_48 = trunc i64 %RIP_56 to i32
    %IP_48 = trunc i64 %RIP_56 to i16
    %RIP_57 = add i64 %RIP_56, 5
    %EIP_49 = trunc i64 %RIP_57 to i32
    %IP_49 = trunc i64 %RIP_57 to i16
    %RBP_7 = load i64, i64* %RBP
    %323 = add i64 %RBP_7, -32
    %324 = inttoptr i64 %323 to double*
    %325 = load double, double* %324, align 1
    %326 = bitcast double %325 to i64
    %ZMM0_11 = load <16 x float>, <16 x float>* %ZMM0
    %327 = bitcast <16 x float> %ZMM0_11 to i512
    %XMM0_11 = trunc i512 %327 to i128
    %328 = zext i64 %326 to i128
    %329 = and i128 %XMM0_11, -18446744073709551616
    %XMM0_12 = or i128 %328, %329
    %330 = bitcast <16 x float> %ZMM0_11 to i512
    %YMM0_11 = trunc i512 %330 to i256
    %331 = zext i128 %XMM0_12 to i256
    %332 = and i256 %YMM0_11, -340282366920938463463374607431768211456
    %YMM0_12 = or i256 %331, %332
    %333 = bitcast <16 x float> %ZMM0_11 to i512
    %334 = zext i128 %XMM0_12 to i512
    %335 = and i512 %333, -340282366920938463463374607431768211456
    %ZMM0_12 = or i512 %334, %335
    %RIP_58 = add i64 %RIP_57, 3
    %EIP_50 = trunc i64 %RIP_58 to i32
    %IP_50 = trunc i64 %RIP_58 to i16
    %336 = add i64 %RBP_7, -20
    %337 = inttoptr i64 %336 to i32*
    %EAX_11 = load i32, i32* %337, align 1
    %RAX_13 = load i64, i64* %RAX
    %RAX_14 = zext i32 %EAX_11 to i64
    %AX_9 = trunc i32 %EAX_11 to i16
    %AL_6 = trunc i32 %EAX_11 to i8
    %338 = lshr i32 %EAX_11, 8
    %AH_3 = trunc i32 %338 to i8
    %RIP_59 = add i64 %RIP_58, 4
    %EIP_51 = trunc i64 %RIP_59 to i32
    %IP_51 = trunc i64 %RIP_59 to i16
    %339 = sitofp i32 %EAX_11 to double
    %340 = bitcast double %339 to i64
    %ZMM1_5 = load <16 x float>, <16 x float>* %ZMM1
    %341 = bitcast <16 x float> %ZMM1_5 to i512
    %XMM1_5 = trunc i512 %341 to i128
    %342 = zext i64 %340 to i128
    %343 = and i128 %XMM1_5, -18446744073709551616
    %XMM1_6 = or i128 %342, %343
    %344 = bitcast <16 x float> %ZMM1_5 to i512
    %YMM1_5 = trunc i512 %344 to i256
    %345 = zext i128 %XMM1_6 to i256
    %346 = and i256 %YMM1_5, -340282366920938463463374607431768211456
    %YMM1_6 = or i256 %345, %346
    %347 = bitcast <16 x float> %ZMM1_5 to i512
    %348 = zext i128 %XMM1_6 to i512
    %349 = and i512 %347, -340282366920938463463374607431768211456
    %ZMM1_6 = or i512 %348, %349
    %RIP_60 = add i64 %RIP_59, 4
    %EIP_52 = trunc i64 %RIP_60 to i32
    %IP_52 = trunc i64 %RIP_60 to i16
    %350 = trunc i128 %XMM1_6 to i64
    %351 = bitcast i64 %350 to double
    %352 = trunc i128 %XMM0_12 to i64
    %353 = bitcast i64 %352 to double
    %354 = fadd double %351, %353
    %355 = bitcast double %354 to i64
    %356 = zext i64 %355 to i128
    %357 = and i128 %XMM1_6, -18446744073709551616
    %XMM1_7 = or i128 %356, %357
    %358 = zext i128 %XMM1_7 to i256
    %359 = and i256 %YMM1_6, -340282366920938463463374607431768211456
    %YMM1_7 = or i256 %358, %359
    %360 = zext i128 %XMM1_7 to i512
    %361 = and i512 %ZMM1_6, -340282366920938463463374607431768211456
    %ZMM1_7 = or i512 %360, %361
    %RIP_61 = add i64 %RIP_60, 4
    %EIP_53 = trunc i64 %RIP_61 to i32
    %IP_53 = trunc i64 %RIP_61 to i16
    %362 = trunc i128 %XMM1_7 to i64
    %363 = bitcast i64 %362 to double
    %EAX_12 = fptosi double %363 to i32
    %RAX_15 = zext i32 %EAX_12 to i64
    %AX_10 = trunc i32 %EAX_12 to i16
    %AL_7 = trunc i32 %EAX_12 to i8
    %364 = lshr i32 %EAX_12, 8
    %AH_4 = trunc i32 %364 to i8
    %RIP_62 = add i64 %RIP_61, 3
    %EIP_54 = trunc i64 %RIP_62 to i32
    %IP_54 = trunc i64 %RIP_62 to i16
    %365 = add i64 %RBP_7, -20
    %366 = inttoptr i64 %365 to i32*
    store i32 %EAX_12, i32* %366, align 1
    %RIP_63 = add i64 %RIP_62, 5
    %EIP_55 = trunc i64 %RIP_63 to i32
    %IP_55 = trunc i64 %RIP_63 to i16
    %367 = add i64 %RBP_7, -32
    %368 = inttoptr i64 %367 to double*
    %369 = load double, double* %368, align 1
    %EAX_13 = fptosi double %369 to i32
    %RAX_16 = zext i32 %EAX_13 to i64
    %AX_11 = trunc i32 %EAX_13 to i16
    %AL_8 = trunc i32 %EAX_13 to i8
    %370 = lshr i32 %EAX_13, 8
    %AH_5 = trunc i32 %370 to i8
    %RIP_64 = add i64 %RIP_63, 4
    %EIP_56 = trunc i64 %RIP_64 to i32
    %IP_56 = trunc i64 %RIP_64 to i16
    %371 = sitofp i32 %EAX_13 to double
    %372 = bitcast double %371 to i64
    %373 = zext i64 %372 to i128
    %374 = and i128 %XMM0_12, -18446744073709551616
    %XMM0_13 = or i128 %373, %374
    %375 = zext i128 %XMM0_13 to i256
    %376 = and i256 %YMM0_12, -340282366920938463463374607431768211456
    %YMM0_13 = or i256 %375, %376
    %377 = zext i128 %XMM0_13 to i512
    %378 = and i512 %ZMM0_12, -340282366920938463463374607431768211456
    %ZMM0_13 = or i512 %377, %378
    %RIP_65 = add i64 %RIP_64, 5
    %EIP_57 = trunc i64 %RIP_65 to i32
    %IP_57 = trunc i64 %RIP_65 to i16
    %379 = add i64 %RBP_7, -16
    %380 = inttoptr i64 %379 to double*
    %381 = load double, double* %380, align 1
    %382 = bitcast double %381 to i64
    %383 = zext i64 %382 to i128
    %384 = and i128 %XMM1_7, -18446744073709551616
    %XMM1_8 = or i128 %383, %384
    %385 = zext i128 %XMM1_8 to i256
    %386 = and i256 %YMM1_7, -340282366920938463463374607431768211456
    %YMM1_8 = or i256 %385, %386
    %387 = zext i128 %XMM1_8 to i512
    %388 = and i512 %ZMM1_7, -340282366920938463463374607431768211456
    %ZMM1_8 = or i512 %387, %388
    %RIP_66 = add i64 %RIP_65, 4
    %EIP_58 = trunc i64 %RIP_66 to i32
    %IP_58 = trunc i64 %RIP_66 to i16
    %389 = trunc i128 %XMM1_8 to i64
    %390 = bitcast i64 %389 to double
    %391 = trunc i128 %XMM0_13 to i64
    %392 = bitcast i64 %391 to double
    %393 = fsub double %390, %392
    %394 = bitcast double %393 to i64
    %395 = zext i64 %394 to i128
    %396 = and i128 %XMM1_8, -18446744073709551616
    %XMM1_9 = or i128 %395, %396
    %397 = zext i128 %XMM1_9 to i256
    %398 = and i256 %YMM1_8, -340282366920938463463374607431768211456
    %YMM1_9 = or i256 %397, %398
    %399 = zext i128 %XMM1_9 to i512
    %400 = and i512 %ZMM1_8, -340282366920938463463374607431768211456
    %ZMM1_9 = or i512 %399, %400
    %RIP_67 = add i64 %RIP_66, 5
    %EIP_59 = trunc i64 %RIP_67 to i32
    %IP_59 = trunc i64 %RIP_67 to i16
    %401 = trunc i128 %XMM1_9 to i64
    %402 = bitcast i64 %401 to double
    %403 = add i64 %RBP_7, -16
    %404 = inttoptr i64 %403 to double*
    store double %402, double* %404, align 1
    %RIP_68 = add i64 %RIP_67, 5
    %EIP_60 = trunc i64 %RIP_68 to i32
    %IP_60 = trunc i64 %RIP_68 to i16
    %405 = add i64 %RBP_7, -32
    %406 = inttoptr i64 %405 to double*
    %407 = load double, double* %406, align 1
    %408 = bitcast double %407 to i64
    %409 = zext i64 %408 to i128
    %410 = and i128 %XMM0_13, -18446744073709551616
    %XMM0_14 = or i128 %409, %410
    %411 = zext i128 %XMM0_14 to i256
    %412 = and i256 %YMM0_13, -340282366920938463463374607431768211456
    %YMM0_14 = or i256 %411, %412
    %413 = zext i128 %XMM0_14 to i512
    %414 = and i512 %ZMM0_13, -340282366920938463463374607431768211456
    %ZMM0_14 = or i512 %413, %414
    %RIP_69 = add i64 %RIP_68, 3
    %EIP_61 = trunc i64 %RIP_69 to i32
    %IP_61 = trunc i64 %RIP_69 to i16
    %415 = add i64 %RBP_7, -20
    %416 = inttoptr i64 %415 to i32*
    %ESI_2 = load i32, i32* %416, align 1
    %RSI_4 = load i64, i64* %RSI
    %RSI_5 = zext i32 %ESI_2 to i64
    %SI_2 = trunc i32 %ESI_2 to i16
    %SIL_2 = trunc i32 %ESI_2 to i8
    %RIP_70 = add i64 %RIP_69, 5
    %EIP_62 = trunc i64 %RIP_70 to i32
    %IP_62 = trunc i64 %RIP_70 to i16
    %417 = add i64 %RBP_7, -16
    %418 = inttoptr i64 %417 to double*
    %419 = load double, double* %418, align 1
    %420 = bitcast double %419 to i64
    %421 = zext i64 %420 to i128
    %422 = and i128 %XMM1_9, -18446744073709551616
    %XMM1_10 = or i128 %421, %422
    %423 = zext i128 %XMM1_10 to i256
    %424 = and i256 %YMM1_9, -340282366920938463463374607431768211456
    %YMM1_10 = or i256 %423, %424
    %425 = zext i128 %XMM1_10 to i512
    %426 = and i512 %ZMM1_9, -340282366920938463463374607431768211456
    %ZMM1_10 = or i512 %425, %426
    %RIP_71 = add i64 %RIP_70, 2
    %EIP_63 = trunc i64 %RIP_71 to i32
    %IP_63 = trunc i64 %RIP_71 to i16
    %427 = and i16 %AX_11, -256
    %AX_12 = or i16 2, %427
    %428 = and i32 %EAX_13, -256
    %EAX_14 = or i32 2, %428
    %429 = and i64 %RAX_16, -256
    %RAX_17 = or i64 2, %429
    %RIP_72 = add i64 %RIP_71, 5
    %EIP_64 = trunc i64 %RIP_72 to i32
    %IP_64 = trunc i64 %RIP_72 to i16
    %RSP_12 = load i64, i64* %RSP
    %RSP_13 = sub i64 %RSP_12, 8
    %430 = inttoptr i64 %RSP_13 to i64*
    store i64 4195797, i64* %430
    %ESP_8 = trunc i64 %RSP_13 to i32
    %SP_8 = trunc i64 %RSP_13 to i16
    %SPL_8 = trunc i64 %RSP_13 to i8
    store i8 %AH_5, i8* %AH
    store i8 2, i8* %AL
    store i16 %AX_12, i16* %AX
    store i16 1723, i16* %DI
    store i8 -69, i8* %DIL
    store i32 %EAX_14, i32* %EAX
    store i32 4196027, i32* %EDI
    store i32 %EIP_64, i32* %EIP
    store i32 %ESI_2, i32* %ESI
    store i32 %ESP_8, i32* %ESP
    store i16 %IP_64, i16* %IP
    store i64 %RAX_17, i64* %RAX
    store i64 %RBP_7, i64* %RBP
    store i64 4196027, i64* %RDI
    store i64 %RIP_72, i64* %RIP
    store i64 %RSI_5, i64* %RSI
    store i64 %RSP_13, i64* %RSP
    store i16 %SI_2, i16* %SI
    store i8 %SIL_2, i8* %SIL
    store i16 %SP_8, i16* %SP
    store i8 %SPL_8, i8* %SPL
    %431 = bitcast i128 %XMM0_14 to <4 x float>
    store <4 x float> %431, <4 x float>* %XMM0
    %432 = bitcast i128 %XMM1_10 to <4 x float>
    store <4 x float> %432, <4 x float>* %XMM1
    %433 = bitcast i256 %YMM0_14 to <8 x float>
    store <8 x float> %433, <8 x float>* %YMM0
    %434 = bitcast i256 %YMM1_10 to <8 x float>
    store <8 x float> %434, <8 x float>* %YMM1
    %435 = bitcast i512 %ZMM0_14 to <16 x float>
    store <16 x float> %435, <16 x float>* %ZMM0
    %436 = bitcast i512 %ZMM1_10 to <16 x float>
    store <16 x float> %436, <16 x float>* %ZMM1
    %437 = load i32, i32* %CtlSysEFLAGS
    store i32 %437, i32* %CtlSysEFLAGS_ptr
    %438 = load i32, i32* %EFLAGS
    store i32 %438, i32* %EFLAGS_ptr
    %439 = load i64, i64* %RAX
    store i64 %439, i64* %RAX_ptr
    %440 = load i64, i64* %RBP
    store i64 %440, i64* %RBP_ptr
    %441 = load i64, i64* %RCX
    store i64 %441, i64* %RCX_ptr
    %442 = load i64, i64* %RDI
    store i64 %442, i64* %RDI_ptr
    %443 = load i64, i64* %RIP
    store i64 %443, i64* %RIP_ptr
    %444 = load i64, i64* %RSI
    store i64 %444, i64* %RSI_ptr
    %445 = load i64, i64* %RSP
    store i64 %445, i64* %RSP_ptr
    %446 = load <16 x float>, <16 x float>* %ZMM0
    store <16 x float> %446, <16 x float>* %ZMM0_ptr
    %447 = load <16 x float>, <16 x float>* %ZMM1
    store <16 x float> %447, <16 x float>* %ZMM1_ptr
    call void @fn_4003D0(%regset* %0)
    %448 = load i32, i32* %CtlSysEFLAGS_ptr
    store i32 %448, i32* %CtlSysEFLAGS
    %449 = load i32, i32* %EFLAGS_ptr
    store i32 %449, i32* %EFLAGS
    %450 = load i64, i64* %RAX_ptr
    store i64 %450, i64* %RAX
    %451 = load i64, i64* %RBP_ptr
    store i64 %451, i64* %RBP
    %452 = load i64, i64* %RCX_ptr
    store i64 %452, i64* %RCX
    %453 = load i64, i64* %RDI_ptr
    store i64 %453, i64* %RDI
    %454 = load i64, i64* %RIP_ptr
    store i64 %454, i64* %RIP
    %455 = load i64, i64* %RSI_ptr
    store i64 %455, i64* %RSI
    %456 = load i64, i64* %RSP_ptr
    store i64 %456, i64* %RSP
    %457 = load <16 x float>, <16 x float>* %ZMM0_ptr
    store <16 x float> %457, <16 x float>* %ZMM0
    %458 = load <16 x float>, <16 x float>* %ZMM1_ptr
    store <16 x float> %458, <16 x float>* %ZMM1
    %RIP_73 = load i64, i64* %RIP
    %RIP_74 = add i64 %RIP_73, 3
    %EIP_65 = trunc i64 %RIP_74 to i32
    %IP_65 = trunc i64 %RIP_74 to i16
    %RAX_18 = load i64, i64* %RAX
    %EAX_15 = trunc i64 %RAX_18 to i32
    %RBP_8 = load i64, i64* %RBP
    %459 = add i64 %RBP_8, -44
    %460 = inttoptr i64 %459 to i32*
    store i32 %EAX_15, i32* %460, align 1
    store i32 %EAX_15, i32* %EAX
    store i32 %EIP_65, i32* %EIP
    store i16 %IP_65, i16* %IP
    store i64 %RAX_18, i64* %RAX
    store i64 %RBP_8, i64* %RBP
    store i64 %RIP_74, i64* %RIP
    br label %bb_4005D8

  bb_4005D8:                                        ; preds = %bb_400589, %bb_400554
    %RIP_76 = add i64 4195800, 5
    %EIP_66 = trunc i64 %RIP_76 to i32
    %IP_66 = trunc i64 %RIP_76 to i16
    store i32 4195805, i32* %EIP
    store i16 1501, i16* %IP
    store i64 4195805, i64* %RIP
    br label %bb_4005DD

  bb_4005DD:                                        ; preds = %bb_4005D8
    %RIP_79 = add i64 4195805, 3
    %EIP_68 = trunc i64 %RIP_79 to i32
    %IP_68 = trunc i64 %RIP_79 to i16
    %RBP_9 = load i64, i64* %RBP
    %461 = add i64 %RBP_9, -24
    %462 = inttoptr i64 %461 to i32*
    %EAX_16 = load i32, i32* %462, align 1
    %RAX_19 = load i64, i64* %RAX
    %RAX_20 = zext i32 %EAX_16 to i64
    %AX_13 = trunc i32 %EAX_16 to i16
    %AL_10 = trunc i32 %EAX_16 to i8
    %463 = lshr i32 %EAX_16, 8
    %AH_6 = trunc i32 %463 to i8
    %RIP_80 = add i64 %RIP_79, 3
    %EIP_69 = trunc i64 %RIP_80 to i32
    %IP_69 = trunc i64 %RIP_80 to i16
    %EAX_17 = add i32 %EAX_16, 1
    %RAX_21 = zext i32 %EAX_17 to i64
    %AX_14 = trunc i32 %EAX_17 to i16
    %AL_11 = trunc i32 %EAX_17 to i8
    %464 = lshr i32 %EAX_17, 8
    %AH_7 = trunc i32 %464 to i8
    %EFLAGS_5 = load i32, i32* %EFLAGS
    %RIP_81 = add i64 %RIP_80, 3
    %EIP_70 = trunc i64 %RIP_81 to i32
    %IP_70 = trunc i64 %RIP_81 to i16
    %465 = add i64 %RBP_9, -24
    %466 = inttoptr i64 %465 to i32*
    store i32 %EAX_17, i32* %466, align 1
    %RIP_82 = add i64 %RIP_81, 5
    %EIP_71 = trunc i64 %RIP_82 to i32
    %IP_71 = trunc i64 %RIP_82 to i16
    %ZF_09 = icmp eq i32 %EAX_17, 0
    %SF_010 = icmp slt i32 %EAX_17, 0
    %467 = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %EAX_16, i32 1)
    %OF_011 = extractvalue { i32, i1 } %467, 1
    %468 = call { i32, i1 } @llvm.uadd.with.overflow.i32(i32 %EAX_16, i32 1)
    %CF_012 = extractvalue { i32, i1 } %468, 1
    %469 = trunc i32 %EAX_17 to i8
    %470 = call i8 @llvm.ctpop.i8(i8 %469)
    %471 = trunc i8 %470 to i1
    %PF_013 = icmp eq i1 %471, false
    %CtlSysEFLAGS_3 = load i32, i32* %CtlSysEFLAGS
    %472 = zext i1 %CF_012 to i32
    %473 = shl i32 %472, 0
    %474 = or i32 %473, %CtlSysEFLAGS_3
    %475 = zext i1 %PF_013 to i32
    %476 = shl i32 %475, 2
    %477 = or i32 %476, %474
    %478 = zext i1 false to i32
    %479 = shl i32 %478, 4
    %480 = or i32 %479, %477
    %481 = zext i1 %ZF_09 to i32
    %482 = shl i32 %481, 6
    %483 = or i32 %482, %480
    %484 = zext i1 %SF_010 to i32
    %485 = shl i32 %484, 7
    %486 = or i32 %485, %483
    %487 = zext i1 %OF_011 to i32
    %488 = shl i32 %487, 11
    %EFLAGS_6 = or i32 %488, %486
    store i8 %AH_7, i8* %AH
    store i8 %AL_11, i8* %AL
    store i16 %AX_14, i16* %AX
    store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
    store i32 %EAX_17, i32* %EAX
    store i32 %EFLAGS_6, i32* %EFLAGS
    store i32 4195613, i32* %EIP
    store i16 1309, i16* %IP
    store i64 %RAX_21, i64* %RAX
    store i64 %RBP_9, i64* %RBP
    store i64 4195613, i64* %RIP
    br label %bb_40051D

  bb_4005EB:                                        ; preds = %bb_40051D
    %RIP_30 = add i64 4195819, 10
    %EIP_26 = trunc i64 %RIP_30 to i32
    %IP_26 = trunc i64 %RIP_30 to i16
    %RIP_31 = add i64 %RIP_30, 2
    %EIP_27 = trunc i64 %RIP_31 to i32
    %IP_27 = trunc i64 %RIP_31 to i16
    %RAX_6 = load i64, i64* %RAX
    %AX_4 = trunc i64 %RAX_6 to i16
    %489 = and i16 %AX_4, -256
    %AX_5 = or i16 0, %489
    %EAX_5 = trunc i64 %RAX_6 to i32
    %490 = and i32 %EAX_5, -256
    %EAX_6 = or i32 0, %490
    %491 = and i64 %RAX_6, -256
    %RAX_7 = or i64 0, %491
    %RIP_32 = add i64 %RIP_31, 5
    %EIP_28 = trunc i64 %RIP_32 to i32
    %IP_28 = trunc i64 %RIP_32 to i16
    %RSP_4 = load i64, i64* %RSP
    %RSP_5 = sub i64 %RSP_4, 8
    %492 = inttoptr i64 %RSP_5 to i64*
    store i64 4195836, i64* %492
    %ESP_3 = trunc i64 %RSP_5 to i32
    %SP_3 = trunc i64 %RSP_5 to i16
    %SPL_3 = trunc i64 %RSP_5 to i8
    store i8 0, i8* %AL
    store i16 %AX_5, i16* %AX
    store i16 1745, i16* %DI
    store i8 -47, i8* %DIL
    store i32 %EAX_6, i32* %EAX
    store i32 4196049, i32* %EDI
    store i32 %EIP_28, i32* %EIP
    store i32 %ESP_3, i32* %ESP
    store i16 %IP_28, i16* %IP
    store i64 %RAX_7, i64* %RAX
    store i64 4196049, i64* %RDI
    store i64 %RIP_32, i64* %RIP
    store i64 %RSP_5, i64* %RSP
    store i16 %SP_3, i16* %SP
    store i8 %SPL_3, i8* %SPL
    %493 = load i32, i32* %CtlSysEFLAGS
    store i32 %493, i32* %CtlSysEFLAGS_ptr
    %494 = load i32, i32* %EFLAGS
    store i32 %494, i32* %EFLAGS_ptr
    %495 = load i64, i64* %RAX
    store i64 %495, i64* %RAX_ptr
    %496 = load i64, i64* %RBP
    store i64 %496, i64* %RBP_ptr
    %497 = load i64, i64* %RCX
    store i64 %497, i64* %RCX_ptr
    %498 = load i64, i64* %RDI
    store i64 %498, i64* %RDI_ptr
    %499 = load i64, i64* %RIP
    store i64 %499, i64* %RIP_ptr
    %500 = load i64, i64* %RSI
    store i64 %500, i64* %RSI_ptr
    %501 = load i64, i64* %RSP
    store i64 %501, i64* %RSP_ptr
    %502 = load <16 x float>, <16 x float>* %ZMM0
    store <16 x float> %502, <16 x float>* %ZMM0_ptr
    %503 = load <16 x float>, <16 x float>* %ZMM1
    store <16 x float> %503, <16 x float>* %ZMM1_ptr
    call void @fn_4003D0(%regset* %0)
    %504 = load i32, i32* %CtlSysEFLAGS_ptr
    store i32 %504, i32* %CtlSysEFLAGS
    %505 = load i32, i32* %EFLAGS_ptr
    store i32 %505, i32* %EFLAGS
    %506 = load i64, i64* %RAX_ptr
    store i64 %506, i64* %RAX
    %507 = load i64, i64* %RBP_ptr
    store i64 %507, i64* %RBP
    %508 = load i64, i64* %RCX_ptr
    store i64 %508, i64* %RCX
    %509 = load i64, i64* %RDI_ptr
    store i64 %509, i64* %RDI
    %510 = load i64, i64* %RIP_ptr
    store i64 %510, i64* %RIP
    %511 = load i64, i64* %RSI_ptr
    store i64 %511, i64* %RSI
    %512 = load i64, i64* %RSP_ptr
    store i64 %512, i64* %RSP
    %513 = load <16 x float>, <16 x float>* %ZMM0_ptr
    store <16 x float> %513, <16 x float>* %ZMM0
    %514 = load <16 x float>, <16 x float>* %ZMM1_ptr
    store <16 x float> %514, <16 x float>* %ZMM1
    %RIP_33 = load i64, i64* %RIP
    %RIP_34 = add i64 %RIP_33, 2
    %EIP_29 = trunc i64 %RIP_34 to i32
    %IP_29 = trunc i64 %RIP_34 to i16
    %RCX_0 = load i64, i64* %RCX
    %ECX_0 = trunc i64 %RCX_0 to i32
    %ECX_1 = xor i32 %ECX_0, %ECX_0
    %RCX_1 = zext i32 %ECX_1 to i64
    %CX_0 = trunc i32 %ECX_1 to i16
    %CL_0 = trunc i32 %ECX_1 to i8
    %515 = lshr i32 %ECX_1, 8
    %CH_0 = trunc i32 %515 to i8
    %EFLAGS_3 = load i32, i32* %EFLAGS
    %RIP_35 = add i64 %RIP_34, 3
    %EIP_30 = trunc i64 %RIP_35 to i32
    %IP_30 = trunc i64 %RIP_35 to i16
    %RAX_8 = load i64, i64* %RAX
    %EAX_7 = trunc i64 %RAX_8 to i32
    %RBP_3 = load i64, i64* %RBP
    %516 = add i64 %RBP_3, -48
    %517 = inttoptr i64 %516 to i32*
    store i32 %EAX_7, i32* %517, align 1
    %RIP_36 = add i64 %RIP_35, 2
    %EIP_31 = trunc i64 %RIP_36 to i32
    %IP_31 = trunc i64 %RIP_36 to i16
    %RAX_9 = zext i32 %ECX_1 to i64
    %AX_6 = trunc i32 %ECX_1 to i16
    %AL_4 = trunc i32 %ECX_1 to i8
    %518 = lshr i32 %ECX_1, 8
    %AH_2 = trunc i32 %518 to i8
    %RIP_37 = add i64 %RIP_36, 4
    %EIP_32 = trunc i64 %RIP_37 to i32
    %IP_32 = trunc i64 %RIP_37 to i16
    %RSP_6 = load i64, i64* %RSP
    %RSP_7 = add i64 %RSP_6, 48
    %ESP_4 = trunc i64 %RSP_7 to i32
    %SP_4 = trunc i64 %RSP_7 to i16
    %SPL_4 = trunc i64 %RSP_7 to i8
    %RIP_38 = add i64 %RIP_37, 1
    %EIP_33 = trunc i64 %RIP_38 to i32
    %IP_33 = trunc i64 %RIP_38 to i16
    %RSP_8 = add i64 %RSP_7, 8
    %ESP_5 = trunc i64 %RSP_8 to i32
    %SP_5 = trunc i64 %RSP_8 to i16
    %SPL_5 = trunc i64 %RSP_8 to i8
    %519 = sub i64 %RSP_8, 8
    %520 = inttoptr i64 %519 to i64*
    %RBP_4 = load i64, i64* %520, align 1
    %EBP_1 = trunc i64 %RBP_4 to i32
    %BP_1 = trunc i64 %RBP_4 to i16
    %BPL_1 = trunc i64 %RBP_4 to i8
    %RIP_39 = add i64 %RIP_38, 1
    %EIP_34 = trunc i64 %RIP_39 to i32
    %IP_34 = trunc i64 %RIP_39 to i16
    %RSP_9 = add i64 %RSP_8, 8
    %521 = inttoptr i64 %RSP_8 to i64*
    %RIP_40 = load i64, i64* %521
    %ESP_6 = trunc i64 %RSP_9 to i32
    %SP_6 = trunc i64 %RSP_9 to i16
    %SPL_6 = trunc i64 %RSP_9 to i8
    %EIP_35 = trunc i64 %RIP_40 to i32
    %IP_35 = trunc i64 %RIP_40 to i16
    %ZF_04 = icmp eq i64 %RSP_7, 0
    %SF_05 = icmp slt i64 %RSP_7, 0
    %522 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_6, i64 48)
    %OF_06 = extractvalue { i64, i1 } %522, 1
    %523 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_6, i64 48)
    %CF_07 = extractvalue { i64, i1 } %523, 1
    %524 = trunc i64 %RSP_7 to i8
    %525 = call i8 @llvm.ctpop.i8(i8 %524)
    %526 = trunc i8 %525 to i1
    %PF_08 = icmp eq i1 %526, false
    %CtlSysEFLAGS_2 = load i32, i32* %CtlSysEFLAGS
    %527 = zext i1 %CF_07 to i32
    %528 = shl i32 %527, 0
    %529 = or i32 %528, %CtlSysEFLAGS_2
    %530 = zext i1 %PF_08 to i32
    %531 = shl i32 %530, 2
    %532 = or i32 %531, %529
    %533 = zext i1 false to i32
    %534 = shl i32 %533, 4
    %535 = or i32 %534, %532
    %536 = zext i1 %ZF_04 to i32
    %537 = shl i32 %536, 6
    %538 = or i32 %537, %535
    %539 = zext i1 %SF_05 to i32
    %540 = shl i32 %539, 7
    %541 = or i32 %540, %538
    %542 = zext i1 %OF_06 to i32
    %543 = shl i32 %542, 11
    %EFLAGS_4 = or i32 %543, %541
    store i8 %AH_2, i8* %AH
    store i8 %AL_4, i8* %AL
    store i16 %AX_6, i16* %AX
    store i16 %BP_1, i16* %BP
    store i8 %BPL_1, i8* %BPL
    store i8 %CH_0, i8* %CH
    store i8 %CL_0, i8* %CL
    store i16 %CX_0, i16* %CX
    store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
    store i32 %ECX_1, i32* %EAX
    store i32 %EBP_1, i32* %EBP
    store i32 %ECX_1, i32* %ECX
    store i32 %EFLAGS_4, i32* %EFLAGS
    store i32 %EIP_35, i32* %EIP
    store i32 %ESP_6, i32* %ESP
    store i16 %IP_35, i16* %IP
    store i64 %RAX_9, i64* %RAX
    store i64 %RBP_4, i64* %RBP
    store i64 %RCX_1, i64* %RCX
    store i64 %RIP_40, i64* %RIP
    store i64 %RSP_9, i64* %RSP
    store i16 %SP_6, i16* %SP
    store i8 %SPL_6, i8* %SPL
    br label %exit_fn_4004D0
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

define void @fn_4003D0(%regset* noalias nocapture) {
  entry_fn_4003D0:
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
    br label %bb_4003D0

  exit_fn_4003D0:                                   ; preds = %bb_4003D0
    %1 = load i64, i64* %RIP
    store i64 %1, i64* %RIP_ptr
    ret void

  bb_4003D0:                                        ; preds = %entry_fn_4003D0
    %RIP_1 = add i64 4195280, 6
    %EIP_0 = trunc i64 %RIP_1 to i32
    %IP_0 = trunc i64 %RIP_1 to i16
    %2 = add i64 %RIP_1, 2100290
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
    br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.uadd.with.overflow.i32(i32, i32) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.usub.with.overflow.i32(i32, i32) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_4004D0(%regset* %3)
  %6 = call i32 @main_fini_regset(%regset* %3)
  ret i32 %6
}

define void @main_init_regset(%regset*, i8*, i32, i32, i8**) {
  %6 = ptrtoint i8* %1 to i64
  %7 = zext i32 %2 to i64
  %8 = add i64 %6, %7
  %9 = sub i64 %8, 8
  %10 = inttoptr i64 %9 to i64*
  store i64 -1, i64* %10
  %11 = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
  store i64 %9, i64* %11
  %12 = zext i32 %3 to i64
  %13 = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
  store i64 %12, i64* %13
  %14 = ptrtoint i8** %4 to i64
  %15 = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
  store i64 %14, i64* %15
  %16 = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  store i32 514, i32* %16
  %17 = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  store i32 514, i32* %17
  ret void
}

define i32 @main_fini_regset(%regset*) {
  %2 = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %3 = load i64, i64* %2
  %4 = trunc i64 %3 to i32
  ret i32 %4
}

define void @fn_400420(%regset* noalias nocapture) {
entry_fn_400420:
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
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %1 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %1 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %EBP_init = trunc i64 %RBP_init to i32
  %EBP = alloca i32
  store i32 %EBP_init, i32* %EBP
  %BP_init = trunc i64 %RBP_init to i16
  %BP = alloca i16
  store i16 %BP_init, i16* %BP
  %BPL_init = trunc i64 %RBP_init to i8
  %BPL = alloca i8
  store i8 %BPL_init, i8* %BPL
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
  br label %bb_400420

exit_fn_400420:                                   ; preds = %bb_40043B, %bb_400448
  %2 = load i32, i32* %CtlSysEFLAGS
  store i32 %2, i32* %CtlSysEFLAGS_ptr
  %3 = load i32, i32* %EFLAGS
  store i32 %3, i32* %EFLAGS_ptr
  %4 = load i64, i64* %RAX
  store i64 %4, i64* %RAX_ptr
  %5 = load i64, i64* %RBP
  store i64 %5, i64* %RBP_ptr
  %6 = load i64, i64* %RDI
  store i64 %6, i64* %RDI_ptr
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  %8 = load i64, i64* %RSP
  store i64 %8, i64* %RSP_ptr
  ret void

bb_400420:                                        ; preds = %entry_fn_400420
  %RIP_1 = add i64 4195360, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %9 = sub i64 %RSP_0, 8
  %10 = inttoptr i64 %9 to i64*
  store i64 %RBP_0, i64* %10, align 1
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %RIP_2 = add i64 %RIP_1, 5
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %RAX_0 = load i64, i64* %RAX
  %RIP_3 = add i64 %RIP_2, 6
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %CC_A_0 = icmp ugt i64 6295600, 6295600
  %CC_AE_0 = icmp uge i64 6295600, 6295600
  %CC_B_0 = icmp ult i64 6295600, 6295600
  %CC_BE_0 = icmp ule i64 6295600, 6295600
  %CC_L_0 = icmp slt i64 6295600, 6295600
  %CC_LE_0 = icmp sle i64 6295600, 6295600
  %CC_G_0 = icmp sgt i64 6295600, 6295600
  %CC_GE_0 = icmp sge i64 6295600, 6295600
  %CC_E_0 = icmp eq i64 6295600, 6295600
  %CC_NE_0 = icmp ne i64 6295600, 6295600
  %11 = sub i64 6295600, 6295600
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295600, i64 6295600)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295600, i64 6295600)
  %CF_0 = extractvalue { i64, i1 } %13, 1
  %14 = trunc i64 %11 to i8
  %15 = call i8 @llvm.ctpop.i8(i8 %14)
  %16 = trunc i8 %15 to i1
  %PF_0 = icmp eq i1 %16, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %17 = zext i1 %CF_0 to i32
  %18 = shl i32 %17, 0
  %19 = or i32 %18, %CtlSysEFLAGS_0
  %20 = zext i1 %PF_0 to i32
  %21 = shl i32 %20, 2
  %22 = or i32 %21, %19
  %23 = zext i1 false to i32
  %24 = shl i32 %23, 4
  %25 = or i32 %24, %22
  %26 = zext i1 %ZF_0 to i32
  %27 = shl i32 %26, 6
  %28 = or i32 %27, %25
  %29 = zext i1 %SF_0 to i32
  %30 = shl i32 %29, 7
  %31 = or i32 %30, %28
  %32 = zext i1 %OF_0 to i32
  %33 = shl i32 %32, 11
  %EFLAGS_0 = or i32 %33, %31
  %RIP_4 = add i64 %RIP_3, 3
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %EBP_0 = trunc i64 %RSP_1 to i32
  %BP_0 = trunc i64 %RSP_1 to i16
  %BPL_0 = trunc i64 %RSP_1 to i8
  %RIP_5 = add i64 %RIP_4, 2
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  store i8 16, i8* %AH
  store i8 48, i8* %AL
  store i16 4144, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295600, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195400, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1096, i16* %IP
  store i64 6295600, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195400, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400448, label %bb_400431

bb_400431:                                        ; preds = %bb_400420
  %RIP_8 = add i64 4195377, 5
  %EIP_6 = trunc i64 %RIP_8 to i32
  %IP_6 = trunc i64 %RIP_8 to i16
  %RAX_2 = load i64, i64* %RAX
  %RIP_9 = add i64 %RIP_8, 3
  %EIP_7 = trunc i64 %RIP_9 to i32
  %IP_7 = trunc i64 %RIP_9 to i16
  %34 = and i64 0, 0
  %CC_A_01 = icmp ugt i64 %34, 0
  %CC_AE_02 = icmp uge i64 %34, 0
  %CC_B_03 = icmp ult i64 %34, 0
  %CC_BE_04 = icmp ule i64 %34, 0
  %CC_L_05 = icmp slt i64 %34, 0
  %CC_LE_06 = icmp sle i64 %34, 0
  %CC_G_07 = icmp sgt i64 %34, 0
  %CC_GE_08 = icmp sge i64 %34, 0
  %CC_E_09 = icmp eq i64 %34, 0
  %CC_NE_010 = icmp ne i64 %34, 0
  %35 = sub i64 %34, 0
  %ZF_011 = icmp eq i64 %35, 0
  %SF_012 = icmp slt i64 %35, 0
  %36 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %34, i64 0)
  %OF_013 = extractvalue { i64, i1 } %36, 1
  %37 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %34, i64 0)
  %CF_014 = extractvalue { i64, i1 } %37, 1
  %38 = trunc i64 %35 to i8
  %39 = call i8 @llvm.ctpop.i8(i8 %38)
  %40 = trunc i8 %39 to i1
  %PF_015 = icmp eq i1 %40, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %41 = zext i1 %CF_014 to i32
  %42 = shl i32 %41, 0
  %43 = or i32 %42, %CtlSysEFLAGS_1
  %44 = zext i1 %PF_015 to i32
  %45 = shl i32 %44, 2
  %46 = or i32 %45, %43
  %47 = zext i1 false to i32
  %48 = shl i32 %47, 4
  %49 = or i32 %48, %46
  %50 = zext i1 %ZF_011 to i32
  %51 = shl i32 %50, 6
  %52 = or i32 %51, %49
  %53 = zext i1 %SF_012 to i32
  %54 = shl i32 %53, 7
  %55 = or i32 %54, %52
  %56 = zext i1 %OF_013 to i32
  %57 = shl i32 %56, 11
  %EFLAGS_1 = or i32 %57, %55
  %RIP_10 = add i64 %RIP_9, 2
  %EIP_8 = trunc i64 %RIP_10 to i32
  %IP_8 = trunc i64 %RIP_10 to i16
  store i8 0, i8* %AH
  store i8 0, i8* %AL
  store i16 0, i16* %AX
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 0, i32* %EAX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 4195400, i32* %EIP
  store i16 1096, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195400, i64* %RIP
  br i1 %CC_E_09, label %bb_400448, label %bb_40043B

bb_40043B:                                        ; preds = %bb_400431
  %RIP_17 = add i64 4195387, 1
  %EIP_13 = trunc i64 %RIP_17 to i32
  %IP_13 = trunc i64 %RIP_17 to i16
  %RSP_5 = load i64, i64* %RSP
  %RSP_6 = add i64 %RSP_5, 8
  %ESP_3 = trunc i64 %RSP_6 to i32
  %SP_3 = trunc i64 %RSP_6 to i16
  %SPL_3 = trunc i64 %RSP_6 to i8
  %58 = sub i64 %RSP_6, 8
  %59 = inttoptr i64 %58 to i64*
  %RBP_2 = load i64, i64* %59, align 1
  %EBP_2 = trunc i64 %RBP_2 to i32
  %BP_2 = trunc i64 %RBP_2 to i16
  %BPL_2 = trunc i64 %RBP_2 to i8
  %RIP_18 = add i64 %RIP_17, 5
  %EIP_14 = trunc i64 %RIP_18 to i32
  %IP_14 = trunc i64 %RIP_18 to i16
  %RDI_0 = load i64, i64* %RDI
  %RIP_19 = add i64 %RIP_18, 2
  %EIP_15 = trunc i64 %RIP_19 to i32
  %IP_15 = trunc i64 %RIP_19 to i16
  %RAX_4 = load i64, i64* %RAX
  %EIP_16 = trunc i64 %RAX_4 to i32
  %IP_16 = trunc i64 %RAX_4 to i16
  %60 = inttoptr i64 %RAX_4 to i8*
  %61 = call i8* @llvm.dc.translate.at(i8* %60)
  %62 = bitcast i8* %61 to void (%regset*)*
  store i16 %BP_2, i16* %BP
  store i8 %BPL_2, i8* %BPL
  store i16 4144, i16* %DI
  store i8 48, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295600, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295600, i64* %RDI
  store i64 %RAX_4, i64* %RIP
  store i64 %RSP_6, i64* %RSP
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
  %67 = load i64, i64* %RDI
  store i64 %67, i64* %RDI_ptr
  %68 = load i64, i64* %RIP
  store i64 %68, i64* %RIP_ptr
  %69 = load i64, i64* %RSP
  store i64 %69, i64* %RSP_ptr
  call void %62(%regset* %0)
  %70 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %70, i32* %CtlSysEFLAGS
  %71 = load i32, i32* %EFLAGS_ptr
  store i32 %71, i32* %EFLAGS
  %72 = load i64, i64* %RAX_ptr
  store i64 %72, i64* %RAX
  %73 = load i64, i64* %RBP_ptr
  store i64 %73, i64* %RBP
  %74 = load i64, i64* %RDI_ptr
  store i64 %74, i64* %RDI
  %75 = load i64, i64* %RIP_ptr
  store i64 %75, i64* %RIP
  %76 = load i64, i64* %RSP_ptr
  store i64 %76, i64* %RSP
  br label %exit_fn_400420

bb_400448:                                        ; preds = %bb_400431, %bb_400420
  %RIP_13 = add i64 4195400, 1
  %EIP_10 = trunc i64 %RIP_13 to i32
  %IP_10 = trunc i64 %RIP_13 to i16
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = add i64 %RSP_2, 8
  %ESP_1 = trunc i64 %RSP_3 to i32
  %SP_1 = trunc i64 %RSP_3 to i16
  %SPL_1 = trunc i64 %RSP_3 to i8
  %77 = sub i64 %RSP_3, 8
  %78 = inttoptr i64 %77 to i64*
  %RBP_1 = load i64, i64* %78, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_14 = add i64 %RIP_13, 1
  %EIP_11 = trunc i64 %RIP_14 to i32
  %IP_11 = trunc i64 %RIP_14 to i16
  %RSP_4 = add i64 %RSP_3, 8
  %79 = inttoptr i64 %RSP_3 to i64*
  %RIP_15 = load i64, i64* %79
  %ESP_2 = trunc i64 %RSP_4 to i32
  %SP_2 = trunc i64 %RSP_4 to i16
  %SPL_2 = trunc i64 %RSP_4 to i8
  %EIP_12 = trunc i64 %RIP_15 to i32
  %IP_12 = trunc i64 %RIP_15 to i16
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %EBP_1, i32* %EBP
  store i32 %EIP_12, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_12, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 %RIP_15, i64* %RIP
  store i64 %RSP_4, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br label %exit_fn_400420
}

define void @fn_400450(%regset* noalias nocapture) {
entry_fn_400450:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %EBP_init = trunc i64 %RBP_init to i32
  %EBP = alloca i32
  store i32 %EBP_init, i32* %EBP
  %BP_init = trunc i64 %RBP_init to i16
  %BP = alloca i16
  store i16 %BP_init, i16* %BP
  %BPL_init = trunc i64 %RBP_init to i8
  %BPL = alloca i8
  store i8 %BPL_init, i8* %BPL
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %1 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %1 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
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
  br label %bb_400450

exit_fn_400450:                                   ; preds = %bb_40047D, %bb_400488
  %2 = load i32, i32* %CtlSysEFLAGS
  store i32 %2, i32* %CtlSysEFLAGS_ptr
  %3 = load i32, i32* %EFLAGS
  store i32 %3, i32* %EFLAGS_ptr
  %4 = load i64, i64* %RAX
  store i64 %4, i64* %RAX_ptr
  %5 = load i64, i64* %RBP
  store i64 %5, i64* %RBP_ptr
  %6 = load i64, i64* %RDI
  store i64 %6, i64* %RDI_ptr
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  %8 = load i64, i64* %RSI
  store i64 %8, i64* %RSI_ptr
  %9 = load i64, i64* %RSP
  store i64 %9, i64* %RSP_ptr
  ret void

bb_400450:                                        ; preds = %entry_fn_400450
  %RIP_1 = add i64 4195408, 5
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_2 = add i64 %RIP_1, 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %10 = sub i64 %RSP_0, 8
  %11 = inttoptr i64 %10 to i64*
  store i64 %RBP_0, i64* %11, align 1
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %RIP_3 = add i64 %RIP_2, 7
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RSI_2 = sub i64 6295600, 6295600
  %ESI_1 = trunc i64 %RSI_2 to i32
  %SI_1 = trunc i64 %RSI_2 to i16
  %SIL_1 = trunc i64 %RSI_2 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 3
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %EBP_0 = trunc i64 %RSP_1 to i32
  %BP_0 = trunc i64 %RSP_1 to i16
  %BPL_0 = trunc i64 %RSP_1 to i8
  %RIP_5 = add i64 %RIP_4, 4
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %12 = zext i8 3 to i64
  %RSI_3 = ashr i64 %RSI_2, %12
  %ESI_2 = trunc i64 %RSI_3 to i32
  %SI_2 = trunc i64 %RSI_3 to i16
  %SIL_2 = trunc i64 %RSI_3 to i8
  %RIP_6 = add i64 %RIP_5, 3
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %EAX_0 = trunc i64 %RSI_3 to i32
  %AX_0 = trunc i64 %RSI_3 to i16
  %AL_0 = trunc i64 %RSI_3 to i8
  %13 = lshr i64 %RSI_3, 8
  %AH_0 = trunc i64 %13 to i8
  %RIP_7 = add i64 %RIP_6, 4
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %14 = zext i8 63 to i64
  %RAX_0 = lshr i64 %RSI_3, %14
  %EAX_1 = trunc i64 %RAX_0 to i32
  %AX_1 = trunc i64 %RAX_0 to i16
  %AL_1 = trunc i64 %RAX_0 to i8
  %15 = lshr i64 %RAX_0, 8
  %AH_1 = trunc i64 %15 to i8
  %RIP_8 = add i64 %RIP_7, 3
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RSI_4 = add i64 %RSI_3, %RAX_0
  %ESI_3 = trunc i64 %RSI_4 to i32
  %SI_3 = trunc i64 %RSI_4 to i16
  %SIL_3 = trunc i64 %RSI_4 to i8
  %RIP_9 = add i64 %RIP_8, 3
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %16 = zext i8 1 to i64
  %RSI_5 = ashr i64 %RSI_4, %16
  %ESI_4 = trunc i64 %RSI_5 to i32
  %SI_4 = trunc i64 %RSI_5 to i16
  %SIL_4 = trunc i64 %RSI_5 to i8
  %RIP_10 = add i64 %RIP_9, 2
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %ZF_0 = icmp eq i64 %RSI_5, 0
  %SF_0 = icmp slt i64 %RSI_5, 0
  %17 = trunc i64 %RSI_5 to i8
  %18 = call i8 @llvm.ctpop.i8(i8 %17)
  %19 = trunc i8 %18 to i1
  %PF_0 = icmp eq i1 %19, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %20 = zext i1 false to i32
  %21 = shl i32 %20, 0
  %22 = or i32 %21, %CtlSysEFLAGS_0
  %23 = zext i1 %PF_0 to i32
  %24 = shl i32 %23, 2
  %25 = or i32 %24, %22
  %26 = zext i1 false to i32
  %27 = shl i32 %26, 4
  %28 = or i32 %27, %25
  %29 = zext i1 %ZF_0 to i32
  %30 = shl i32 %29, 6
  %31 = or i32 %30, %28
  %32 = zext i1 %SF_0 to i32
  %33 = shl i32 %32, 7
  %34 = or i32 %33, %31
  %35 = zext i1 false to i32
  %36 = shl i32 %35, 11
  %EFLAGS_1 = or i32 %36, %34
  %37 = lshr i32 %EFLAGS_1, 6
  %ZF_1 = trunc i32 %37 to i1
  store i8 %AH_1, i8* %AH
  store i8 %AL_1, i8* %AL
  store i16 %AX_1, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 4195464, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1160, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195464, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400488, label %bb_400473

bb_400473:                                        ; preds = %bb_400450
  %RIP_13 = add i64 4195443, 5
  %EIP_11 = trunc i64 %RIP_13 to i32
  %IP_11 = trunc i64 %RIP_13 to i16
  %RAX_1 = load i64, i64* %RAX
  %RIP_14 = add i64 %RIP_13, 3
  %EIP_12 = trunc i64 %RIP_14 to i32
  %IP_12 = trunc i64 %RIP_14 to i16
  %38 = and i64 0, 0
  %CC_A_0 = icmp ugt i64 %38, 0
  %CC_AE_0 = icmp uge i64 %38, 0
  %CC_B_0 = icmp ult i64 %38, 0
  %CC_BE_0 = icmp ule i64 %38, 0
  %CC_L_0 = icmp slt i64 %38, 0
  %CC_LE_0 = icmp sle i64 %38, 0
  %CC_G_0 = icmp sgt i64 %38, 0
  %CC_GE_0 = icmp sge i64 %38, 0
  %CC_E_0 = icmp eq i64 %38, 0
  %CC_NE_0 = icmp ne i64 %38, 0
  %39 = sub i64 %38, 0
  %ZF_01 = icmp eq i64 %39, 0
  %SF_02 = icmp slt i64 %39, 0
  %40 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %38, i64 0)
  %OF_0 = extractvalue { i64, i1 } %40, 1
  %41 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %38, i64 0)
  %CF_0 = extractvalue { i64, i1 } %41, 1
  %42 = trunc i64 %39 to i8
  %43 = call i8 @llvm.ctpop.i8(i8 %42)
  %44 = trunc i8 %43 to i1
  %PF_03 = icmp eq i1 %44, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %45 = zext i1 %CF_0 to i32
  %46 = shl i32 %45, 0
  %47 = or i32 %46, %CtlSysEFLAGS_1
  %48 = zext i1 %PF_03 to i32
  %49 = shl i32 %48, 2
  %50 = or i32 %49, %47
  %51 = zext i1 false to i32
  %52 = shl i32 %51, 4
  %53 = or i32 %52, %50
  %54 = zext i1 %ZF_01 to i32
  %55 = shl i32 %54, 6
  %56 = or i32 %55, %53
  %57 = zext i1 %SF_02 to i32
  %58 = shl i32 %57, 7
  %59 = or i32 %58, %56
  %60 = zext i1 %OF_0 to i32
  %61 = shl i32 %60, 11
  %EFLAGS_2 = or i32 %61, %59
  %RIP_15 = add i64 %RIP_14, 2
  %EIP_13 = trunc i64 %RIP_15 to i32
  %IP_13 = trunc i64 %RIP_15 to i16
  store i8 0, i8* %AH
  store i8 0, i8* %AL
  store i16 0, i16* %AX
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 0, i32* %EAX
  store i32 %EFLAGS_2, i32* %EFLAGS
  store i32 4195464, i32* %EIP
  store i16 1160, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195464, i64* %RIP
  br i1 %CC_E_0, label %bb_400488, label %bb_40047D

bb_40047D:                                        ; preds = %bb_400473
  %RIP_22 = add i64 4195453, 1
  %EIP_18 = trunc i64 %RIP_22 to i32
  %IP_18 = trunc i64 %RIP_22 to i16
  %RSP_5 = load i64, i64* %RSP
  %RSP_6 = add i64 %RSP_5, 8
  %ESP_3 = trunc i64 %RSP_6 to i32
  %SP_3 = trunc i64 %RSP_6 to i16
  %SPL_3 = trunc i64 %RSP_6 to i8
  %62 = sub i64 %RSP_6, 8
  %63 = inttoptr i64 %62 to i64*
  %RBP_2 = load i64, i64* %63, align 1
  %EBP_2 = trunc i64 %RBP_2 to i32
  %BP_2 = trunc i64 %RBP_2 to i16
  %BPL_2 = trunc i64 %RBP_2 to i8
  %RIP_23 = add i64 %RIP_22, 5
  %EIP_19 = trunc i64 %RIP_23 to i32
  %IP_19 = trunc i64 %RIP_23 to i16
  %RDI_0 = load i64, i64* %RDI
  %RIP_24 = add i64 %RIP_23, 2
  %EIP_20 = trunc i64 %RIP_24 to i32
  %IP_20 = trunc i64 %RIP_24 to i16
  %RAX_3 = load i64, i64* %RAX
  %EIP_21 = trunc i64 %RAX_3 to i32
  %IP_21 = trunc i64 %RAX_3 to i16
  %64 = inttoptr i64 %RAX_3 to i8*
  %65 = call i8* @llvm.dc.translate.at(i8* %64)
  %66 = bitcast i8* %65 to void (%regset*)*
  store i16 %BP_2, i16* %BP
  store i8 %BPL_2, i8* %BPL
  store i16 4144, i16* %DI
  store i8 48, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295600, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295600, i64* %RDI
  store i64 %RAX_3, i64* %RIP
  store i64 %RSP_6, i64* %RSP
  store i16 %SP_3, i16* %SP
  store i8 %SPL_3, i8* %SPL
  %67 = load i32, i32* %CtlSysEFLAGS
  store i32 %67, i32* %CtlSysEFLAGS_ptr
  %68 = load i32, i32* %EFLAGS
  store i32 %68, i32* %EFLAGS_ptr
  %69 = load i64, i64* %RAX
  store i64 %69, i64* %RAX_ptr
  %70 = load i64, i64* %RBP
  store i64 %70, i64* %RBP_ptr
  %71 = load i64, i64* %RDI
  store i64 %71, i64* %RDI_ptr
  %72 = load i64, i64* %RIP
  store i64 %72, i64* %RIP_ptr
  %73 = load i64, i64* %RSI
  store i64 %73, i64* %RSI_ptr
  %74 = load i64, i64* %RSP
  store i64 %74, i64* %RSP_ptr
  call void %66(%regset* %0)
  %75 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %75, i32* %CtlSysEFLAGS
  %76 = load i32, i32* %EFLAGS_ptr
  store i32 %76, i32* %EFLAGS
  %77 = load i64, i64* %RAX_ptr
  store i64 %77, i64* %RAX
  %78 = load i64, i64* %RBP_ptr
  store i64 %78, i64* %RBP
  %79 = load i64, i64* %RDI_ptr
  store i64 %79, i64* %RDI
  %80 = load i64, i64* %RIP_ptr
  store i64 %80, i64* %RIP
  %81 = load i64, i64* %RSI_ptr
  store i64 %81, i64* %RSI
  %82 = load i64, i64* %RSP_ptr
  store i64 %82, i64* %RSP
  br label %exit_fn_400450

bb_400488:                                        ; preds = %bb_400473, %bb_400450
  %RIP_18 = add i64 4195464, 1
  %EIP_15 = trunc i64 %RIP_18 to i32
  %IP_15 = trunc i64 %RIP_18 to i16
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = add i64 %RSP_2, 8
  %ESP_1 = trunc i64 %RSP_3 to i32
  %SP_1 = trunc i64 %RSP_3 to i16
  %SPL_1 = trunc i64 %RSP_3 to i8
  %83 = sub i64 %RSP_3, 8
  %84 = inttoptr i64 %83 to i64*
  %RBP_1 = load i64, i64* %84, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_19 = add i64 %RIP_18, 1
  %EIP_16 = trunc i64 %RIP_19 to i32
  %IP_16 = trunc i64 %RIP_19 to i16
  %RSP_4 = add i64 %RSP_3, 8
  %85 = inttoptr i64 %RSP_3 to i64*
  %RIP_20 = load i64, i64* %85
  %ESP_2 = trunc i64 %RSP_4 to i32
  %SP_2 = trunc i64 %RSP_4 to i16
  %SPL_2 = trunc i64 %RSP_4 to i8
  %EIP_17 = trunc i64 %RIP_20 to i32
  %IP_17 = trunc i64 %RIP_20 to i16
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %EBP_1, i32* %EBP
  store i32 %EIP_17, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_17, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 %RIP_20, i64* %RIP
  store i64 %RSP_4, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br label %exit_fn_400450
}

define void @fn_400490(%regset* noalias nocapture) {
entry_fn_400490:
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
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
  br label %bb_400490

exit_fn_400490:                                   ; preds = %bb_4004B0, %bb_400499
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RBP
  store i64 %3, i64* %RBP_ptr
  %4 = load i64, i64* %RIP
  store i64 %4, i64* %RIP_ptr
  %5 = load i64, i64* %RSP
  store i64 %5, i64* %RSP_ptr
  ret void

bb_400490:                                        ; preds = %entry_fn_400490
  %RIP_1 = add i64 4195472, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100121
  %7 = inttoptr i64 %6 to i8*
  %8 = load i8, i8* %7, align 1
  %CC_A_0 = icmp ugt i8 %8, 0
  %CC_AE_0 = icmp uge i8 %8, 0
  %CC_B_0 = icmp ult i8 %8, 0
  %CC_BE_0 = icmp ule i8 %8, 0
  %CC_L_0 = icmp slt i8 %8, 0
  %CC_LE_0 = icmp sle i8 %8, 0
  %CC_G_0 = icmp sgt i8 %8, 0
  %CC_GE_0 = icmp sge i8 %8, 0
  %CC_E_0 = icmp eq i8 %8, 0
  %CC_NE_0 = icmp ne i8 %8, 0
  %9 = sub i8 %8, 0
  %ZF_0 = icmp eq i8 %9, 0
  %SF_0 = icmp slt i8 %9, 0
  %10 = call { i8, i1 } @llvm.ssub.with.overflow.i8(i8 %8, i8 0)
  %OF_0 = extractvalue { i8, i1 } %10, 1
  %11 = call { i8, i1 } @llvm.usub.with.overflow.i8(i8 %8, i8 0)
  %CF_0 = extractvalue { i8, i1 } %11, 1
  %12 = call i8 @llvm.ctpop.i8(i8 %9)
  %13 = trunc i8 %12 to i1
  %PF_0 = icmp eq i1 %13, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %14 = zext i1 %CF_0 to i32
  %15 = shl i32 %14, 0
  %16 = or i32 %15, %CtlSysEFLAGS_0
  %17 = zext i1 %PF_0 to i32
  %18 = shl i32 %17, 2
  %19 = or i32 %18, %16
  %20 = zext i1 false to i32
  %21 = shl i32 %20, 4
  %22 = or i32 %21, %19
  %23 = zext i1 %ZF_0 to i32
  %24 = shl i32 %23, 6
  %25 = or i32 %24, %22
  %26 = zext i1 %SF_0 to i32
  %27 = shl i32 %26, 7
  %28 = or i32 %27, %25
  %29 = zext i1 %OF_0 to i32
  %30 = shl i32 %29, 11
  %EFLAGS_0 = or i32 %30, %28
  %RIP_2 = add i64 %RIP_1, 2
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195504, i32* %EIP
  store i16 1200, i16* %IP
  store i64 4195504, i64* %RIP
  br i1 %CC_NE_0, label %bb_4004B0, label %bb_400499

bb_400499:                                        ; preds = %bb_400490
  %RIP_5 = add i64 4195481, 1
  %EIP_3 = trunc i64 %RIP_5 to i32
  %IP_3 = trunc i64 %RIP_5 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %31 = sub i64 %RSP_0, 8
  %32 = inttoptr i64 %31 to i64*
  store i64 %RBP_0, i64* %32, align 1
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %RIP_6 = add i64 %RIP_5, 3
  %EIP_4 = trunc i64 %RIP_6 to i32
  %IP_4 = trunc i64 %RIP_6 to i16
  %EBP_0 = trunc i64 %RSP_1 to i32
  %BP_0 = trunc i64 %RSP_1 to i16
  %BPL_0 = trunc i64 %RSP_1 to i8
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RSP_2 = sub i64 %RSP_1, 8
  %33 = inttoptr i64 %RSP_2 to i64*
  store i64 4195490, i64* %33
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %EBP_0, i32* %EBP
  store i32 %EIP_5, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 %IP_5, i16* %IP
  store i64 %RSP_1, i64* %RBP
  store i64 %RIP_7, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  %34 = load i32, i32* %CtlSysEFLAGS
  store i32 %34, i32* %CtlSysEFLAGS_ptr
  %35 = load i32, i32* %EFLAGS
  store i32 %35, i32* %EFLAGS_ptr
  %36 = load i64, i64* %RBP
  store i64 %36, i64* %RBP_ptr
  %37 = load i64, i64* %RIP
  store i64 %37, i64* %RIP_ptr
  %38 = load i64, i64* %RSP
  store i64 %38, i64* %RSP_ptr
  call void @fn_400420(%regset* %0)
  %39 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %39, i32* %CtlSysEFLAGS
  %40 = load i32, i32* %EFLAGS_ptr
  store i32 %40, i32* %EFLAGS
  %41 = load i64, i64* %RBP_ptr
  store i64 %41, i64* %RBP
  %42 = load i64, i64* %RIP_ptr
  store i64 %42, i64* %RIP
  %43 = load i64, i64* %RSP_ptr
  store i64 %43, i64* %RSP
  %RIP_8 = load i64, i64* %RIP
  %RIP_9 = add i64 %RIP_8, 7
  %EIP_6 = trunc i64 %RIP_9 to i32
  %IP_6 = trunc i64 %RIP_9 to i16
  %44 = add i64 %RIP_9, 2100103
  %45 = inttoptr i64 %44 to i8*
  store i8 1, i8* %45, align 1
  %RIP_10 = add i64 %RIP_9, 1
  %EIP_7 = trunc i64 %RIP_10 to i32
  %IP_7 = trunc i64 %RIP_10 to i16
  %RSP_3 = load i64, i64* %RSP
  %RSP_4 = add i64 %RSP_3, 8
  %ESP_2 = trunc i64 %RSP_4 to i32
  %SP_2 = trunc i64 %RSP_4 to i16
  %SPL_2 = trunc i64 %RSP_4 to i8
  %46 = sub i64 %RSP_4, 8
  %47 = inttoptr i64 %46 to i64*
  %RBP_1 = load i64, i64* %47, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_11 = add i64 %RIP_10, 1
  %EIP_8 = trunc i64 %RIP_11 to i32
  %IP_8 = trunc i64 %RIP_11 to i16
  %RSP_5 = add i64 %RSP_4, 8
  %48 = inttoptr i64 %RSP_4 to i64*
  %RIP_12 = load i64, i64* %48
  %ESP_3 = trunc i64 %RSP_5 to i32
  %SP_3 = trunc i64 %RSP_5 to i16
  %SPL_3 = trunc i64 %RSP_5 to i8
  %EIP_9 = trunc i64 %RIP_12 to i32
  %IP_9 = trunc i64 %RIP_12 to i16
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %EBP_1, i32* %EBP
  store i32 %EIP_9, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_9, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 %RIP_12, i64* %RIP
  store i64 %RSP_5, i64* %RSP
  store i16 %SP_3, i16* %SP
  store i8 %SPL_3, i8* %SPL
  br label %exit_fn_400490

bb_4004B0:                                        ; preds = %bb_400490
  %RIP_14 = add i64 4195504, 1
  %EIP_10 = trunc i64 %RIP_14 to i32
  %IP_10 = trunc i64 %RIP_14 to i16
  %RIP_15 = add i64 %RIP_14, 1
  %EIP_11 = trunc i64 %RIP_15 to i32
  %IP_11 = trunc i64 %RIP_15 to i16
  %RSP_6 = load i64, i64* %RSP
  %RSP_7 = add i64 %RSP_6, 8
  %49 = inttoptr i64 %RSP_6 to i64*
  %RIP_16 = load i64, i64* %49
  %ESP_4 = trunc i64 %RSP_7 to i32
  %SP_4 = trunc i64 %RSP_7 to i16
  %SPL_4 = trunc i64 %RSP_7 to i8
  %EIP_12 = trunc i64 %RIP_16 to i32
  %IP_12 = trunc i64 %RIP_16 to i16
  store i32 %EIP_12, i32* %EIP
  store i32 %ESP_4, i32* %ESP
  store i16 %IP_12, i16* %IP
  store i64 %RIP_16, i64* %RIP
  store i64 %RSP_7, i64* %RSP
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  br label %exit_fn_400490
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %1 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %1 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
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
  br label %bb_4004C0

exit_fn_4004C0:                                   ; preds = %bb_40047D, %bb_400488
  %2 = load i32, i32* %CtlSysEFLAGS
  store i32 %2, i32* %CtlSysEFLAGS_ptr
  %3 = load i32, i32* %EFLAGS
  store i32 %3, i32* %EFLAGS_ptr
  %4 = load i64, i64* %RAX
  store i64 %4, i64* %RAX_ptr
  %5 = load i64, i64* %RBP
  store i64 %5, i64* %RBP_ptr
  %6 = load i64, i64* %RDI
  store i64 %6, i64* %RDI_ptr
  %7 = load i64, i64* %RIP
  store i64 %7, i64* %RIP_ptr
  %8 = load i64, i64* %RSI
  store i64 %8, i64* %RSI_ptr
  %9 = load i64, i64* %RSP
  store i64 %9, i64* %RSP_ptr
  ret void

bb_4004C0:                                        ; preds = %entry_fn_4004C0
  %RIP_1 = add i64 4195520, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %10 = sub i64 %RSP_0, 8
  %11 = inttoptr i64 %10 to i64*
  store i64 %RBP_0, i64* %11, align 1
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
  %RIP_3 = add i64 %RIP_2, 1
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %12 = sub i64 %RSP_2, 8
  %13 = inttoptr i64 %12 to i64*
  %RBP_1 = load i64, i64* %13, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_4 = add i64 %RIP_3, 2
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %EBP_1, i32* %EBP
  store i32 4195408, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1104, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195408, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400450

bb_400450:                                        ; preds = %bb_4004C0
  %RIP_7 = add i64 4195408, 5
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_8 = add i64 %RIP_7, 1
  %EIP_6 = trunc i64 %RIP_8 to i32
  %IP_6 = trunc i64 %RIP_8 to i16
  %RBP_2 = load i64, i64* %RBP
  %RSP_3 = load i64, i64* %RSP
  %14 = sub i64 %RSP_3, 8
  %15 = inttoptr i64 %14 to i64*
  store i64 %RBP_2, i64* %15, align 1
  %RSP_4 = sub i64 %RSP_3, 8
  %ESP_2 = trunc i64 %RSP_4 to i32
  %SP_2 = trunc i64 %RSP_4 to i16
  %SPL_2 = trunc i64 %RSP_4 to i8
  %RIP_9 = add i64 %RIP_8, 7
  %EIP_7 = trunc i64 %RIP_9 to i32
  %IP_7 = trunc i64 %RIP_9 to i16
  %RSI_2 = sub i64 6295600, 6295600
  %ESI_1 = trunc i64 %RSI_2 to i32
  %SI_1 = trunc i64 %RSI_2 to i16
  %SIL_1 = trunc i64 %RSI_2 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_10 = add i64 %RIP_9, 3
  %EIP_8 = trunc i64 %RIP_10 to i32
  %IP_8 = trunc i64 %RIP_10 to i16
  %EBP_2 = trunc i64 %RSP_4 to i32
  %BP_2 = trunc i64 %RSP_4 to i16
  %BPL_2 = trunc i64 %RSP_4 to i8
  %RIP_11 = add i64 %RIP_10, 4
  %EIP_9 = trunc i64 %RIP_11 to i32
  %IP_9 = trunc i64 %RIP_11 to i16
  %16 = zext i8 3 to i64
  %RSI_3 = ashr i64 %RSI_2, %16
  %ESI_2 = trunc i64 %RSI_3 to i32
  %SI_2 = trunc i64 %RSI_3 to i16
  %SIL_2 = trunc i64 %RSI_3 to i8
  %RIP_12 = add i64 %RIP_11, 3
  %EIP_10 = trunc i64 %RIP_12 to i32
  %IP_10 = trunc i64 %RIP_12 to i16
  %EAX_0 = trunc i64 %RSI_3 to i32
  %AX_0 = trunc i64 %RSI_3 to i16
  %AL_0 = trunc i64 %RSI_3 to i8
  %17 = lshr i64 %RSI_3, 8
  %AH_0 = trunc i64 %17 to i8
  %RIP_13 = add i64 %RIP_12, 4
  %EIP_11 = trunc i64 %RIP_13 to i32
  %IP_11 = trunc i64 %RIP_13 to i16
  %18 = zext i8 63 to i64
  %RAX_0 = lshr i64 %RSI_3, %18
  %EAX_1 = trunc i64 %RAX_0 to i32
  %AX_1 = trunc i64 %RAX_0 to i16
  %AL_1 = trunc i64 %RAX_0 to i8
  %19 = lshr i64 %RAX_0, 8
  %AH_1 = trunc i64 %19 to i8
  %RIP_14 = add i64 %RIP_13, 3
  %EIP_12 = trunc i64 %RIP_14 to i32
  %IP_12 = trunc i64 %RIP_14 to i16
  %RSI_4 = add i64 %RSI_3, %RAX_0
  %ESI_3 = trunc i64 %RSI_4 to i32
  %SI_3 = trunc i64 %RSI_4 to i16
  %SIL_3 = trunc i64 %RSI_4 to i8
  %RIP_15 = add i64 %RIP_14, 3
  %EIP_13 = trunc i64 %RIP_15 to i32
  %IP_13 = trunc i64 %RIP_15 to i16
  %20 = zext i8 1 to i64
  %RSI_5 = ashr i64 %RSI_4, %20
  %ESI_4 = trunc i64 %RSI_5 to i32
  %SI_4 = trunc i64 %RSI_5 to i16
  %SIL_4 = trunc i64 %RSI_5 to i8
  %RIP_16 = add i64 %RIP_15, 2
  %EIP_14 = trunc i64 %RIP_16 to i32
  %IP_14 = trunc i64 %RIP_16 to i16
  %ZF_0 = icmp eq i64 %RSI_5, 0
  %SF_0 = icmp slt i64 %RSI_5, 0
  %21 = trunc i64 %RSI_5 to i8
  %22 = call i8 @llvm.ctpop.i8(i8 %21)
  %23 = trunc i8 %22 to i1
  %PF_0 = icmp eq i1 %23, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %24 = zext i1 false to i32
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
  %39 = zext i1 false to i32
  %40 = shl i32 %39, 11
  %EFLAGS_1 = or i32 %40, %38
  %41 = lshr i32 %EFLAGS_1, 6
  %ZF_1 = trunc i32 %41 to i1
  store i8 %AH_1, i8* %AH
  store i8 %AL_1, i8* %AL
  store i16 %AX_1, i16* %AX
  store i16 %BP_2, i16* %BP
  store i8 %BPL_2, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_2, i32* %EBP
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 4195464, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1160, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195464, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400488, label %bb_400473

bb_400473:                                        ; preds = %bb_400450
  %RIP_19 = add i64 4195443, 5
  %EIP_16 = trunc i64 %RIP_19 to i32
  %IP_16 = trunc i64 %RIP_19 to i16
  %RAX_1 = load i64, i64* %RAX
  %RIP_20 = add i64 %RIP_19, 3
  %EIP_17 = trunc i64 %RIP_20 to i32
  %IP_17 = trunc i64 %RIP_20 to i16
  %42 = and i64 0, 0
  %CC_A_0 = icmp ugt i64 %42, 0
  %CC_AE_0 = icmp uge i64 %42, 0
  %CC_B_0 = icmp ult i64 %42, 0
  %CC_BE_0 = icmp ule i64 %42, 0
  %CC_L_0 = icmp slt i64 %42, 0
  %CC_LE_0 = icmp sle i64 %42, 0
  %CC_G_0 = icmp sgt i64 %42, 0
  %CC_GE_0 = icmp sge i64 %42, 0
  %CC_E_0 = icmp eq i64 %42, 0
  %CC_NE_0 = icmp ne i64 %42, 0
  %43 = sub i64 %42, 0
  %ZF_01 = icmp eq i64 %43, 0
  %SF_02 = icmp slt i64 %43, 0
  %44 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %42, i64 0)
  %OF_0 = extractvalue { i64, i1 } %44, 1
  %45 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %42, i64 0)
  %CF_0 = extractvalue { i64, i1 } %45, 1
  %46 = trunc i64 %43 to i8
  %47 = call i8 @llvm.ctpop.i8(i8 %46)
  %48 = trunc i8 %47 to i1
  %PF_03 = icmp eq i1 %48, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %49 = zext i1 %CF_0 to i32
  %50 = shl i32 %49, 0
  %51 = or i32 %50, %CtlSysEFLAGS_1
  %52 = zext i1 %PF_03 to i32
  %53 = shl i32 %52, 2
  %54 = or i32 %53, %51
  %55 = zext i1 false to i32
  %56 = shl i32 %55, 4
  %57 = or i32 %56, %54
  %58 = zext i1 %ZF_01 to i32
  %59 = shl i32 %58, 6
  %60 = or i32 %59, %57
  %61 = zext i1 %SF_02 to i32
  %62 = shl i32 %61, 7
  %63 = or i32 %62, %60
  %64 = zext i1 %OF_0 to i32
  %65 = shl i32 %64, 11
  %EFLAGS_2 = or i32 %65, %63
  %RIP_21 = add i64 %RIP_20, 2
  %EIP_18 = trunc i64 %RIP_21 to i32
  %IP_18 = trunc i64 %RIP_21 to i16
  store i8 0, i8* %AH
  store i8 0, i8* %AL
  store i16 0, i16* %AX
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 0, i32* %EAX
  store i32 %EFLAGS_2, i32* %EFLAGS
  store i32 4195464, i32* %EIP
  store i16 1160, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195464, i64* %RIP
  br i1 %CC_E_0, label %bb_400488, label %bb_40047D

bb_40047D:                                        ; preds = %bb_400473
  %RIP_28 = add i64 4195453, 1
  %EIP_23 = trunc i64 %RIP_28 to i32
  %IP_23 = trunc i64 %RIP_28 to i16
  %RSP_8 = load i64, i64* %RSP
  %RSP_9 = add i64 %RSP_8, 8
  %ESP_5 = trunc i64 %RSP_9 to i32
  %SP_5 = trunc i64 %RSP_9 to i16
  %SPL_5 = trunc i64 %RSP_9 to i8
  %66 = sub i64 %RSP_9, 8
  %67 = inttoptr i64 %66 to i64*
  %RBP_4 = load i64, i64* %67, align 1
  %EBP_4 = trunc i64 %RBP_4 to i32
  %BP_4 = trunc i64 %RBP_4 to i16
  %BPL_4 = trunc i64 %RBP_4 to i8
  %RIP_29 = add i64 %RIP_28, 5
  %EIP_24 = trunc i64 %RIP_29 to i32
  %IP_24 = trunc i64 %RIP_29 to i16
  %RDI_0 = load i64, i64* %RDI
  %RIP_30 = add i64 %RIP_29, 2
  %EIP_25 = trunc i64 %RIP_30 to i32
  %IP_25 = trunc i64 %RIP_30 to i16
  %RAX_3 = load i64, i64* %RAX
  %EIP_26 = trunc i64 %RAX_3 to i32
  %IP_26 = trunc i64 %RAX_3 to i16
  %68 = inttoptr i64 %RAX_3 to i8*
  %69 = call i8* @llvm.dc.translate.at(i8* %68)
  %70 = bitcast i8* %69 to void (%regset*)*
  store i16 %BP_4, i16* %BP
  store i8 %BPL_4, i8* %BPL
  store i16 4144, i16* %DI
  store i8 48, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295600, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295600, i64* %RDI
  store i64 %RAX_3, i64* %RIP
  store i64 %RSP_9, i64* %RSP
  store i16 %SP_5, i16* %SP
  store i8 %SPL_5, i8* %SPL
  %71 = load i32, i32* %CtlSysEFLAGS
  store i32 %71, i32* %CtlSysEFLAGS_ptr
  %72 = load i32, i32* %EFLAGS
  store i32 %72, i32* %EFLAGS_ptr
  %73 = load i64, i64* %RAX
  store i64 %73, i64* %RAX_ptr
  %74 = load i64, i64* %RBP
  store i64 %74, i64* %RBP_ptr
  %75 = load i64, i64* %RDI
  store i64 %75, i64* %RDI_ptr
  %76 = load i64, i64* %RIP
  store i64 %76, i64* %RIP_ptr
  %77 = load i64, i64* %RSI
  store i64 %77, i64* %RSI_ptr
  %78 = load i64, i64* %RSP
  store i64 %78, i64* %RSP_ptr
  call void %70(%regset* %0)
  %79 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %79, i32* %CtlSysEFLAGS
  %80 = load i32, i32* %EFLAGS_ptr
  store i32 %80, i32* %EFLAGS
  %81 = load i64, i64* %RAX_ptr
  store i64 %81, i64* %RAX
  %82 = load i64, i64* %RBP_ptr
  store i64 %82, i64* %RBP
  %83 = load i64, i64* %RDI_ptr
  store i64 %83, i64* %RDI
  %84 = load i64, i64* %RIP_ptr
  store i64 %84, i64* %RIP
  %85 = load i64, i64* %RSI_ptr
  store i64 %85, i64* %RSI
  %86 = load i64, i64* %RSP_ptr
  store i64 %86, i64* %RSP
  br label %exit_fn_4004C0

bb_400488:                                        ; preds = %bb_400473, %bb_400450
  %RIP_24 = add i64 4195464, 1
  %EIP_20 = trunc i64 %RIP_24 to i32
  %IP_20 = trunc i64 %RIP_24 to i16
  %RSP_5 = load i64, i64* %RSP
  %RSP_6 = add i64 %RSP_5, 8
  %ESP_3 = trunc i64 %RSP_6 to i32
  %SP_3 = trunc i64 %RSP_6 to i16
  %SPL_3 = trunc i64 %RSP_6 to i8
  %87 = sub i64 %RSP_6, 8
  %88 = inttoptr i64 %87 to i64*
  %RBP_3 = load i64, i64* %88, align 1
  %EBP_3 = trunc i64 %RBP_3 to i32
  %BP_3 = trunc i64 %RBP_3 to i16
  %BPL_3 = trunc i64 %RBP_3 to i8
  %RIP_25 = add i64 %RIP_24, 1
  %EIP_21 = trunc i64 %RIP_25 to i32
  %IP_21 = trunc i64 %RIP_25 to i16
  %RSP_7 = add i64 %RSP_6, 8
  %89 = inttoptr i64 %RSP_6 to i64*
  %RIP_26 = load i64, i64* %89
  %ESP_4 = trunc i64 %RSP_7 to i32
  %SP_4 = trunc i64 %RSP_7 to i16
  %SPL_4 = trunc i64 %RSP_7 to i8
  %EIP_22 = trunc i64 %RIP_26 to i32
  %IP_22 = trunc i64 %RIP_26 to i16
  store i16 %BP_3, i16* %BP
  store i8 %BPL_3, i8* %BPL
  store i32 %EBP_3, i32* %EBP
  store i32 %EIP_22, i32* %EIP
  store i32 %ESP_4, i32* %ESP
  store i16 %IP_22, i16* %IP
  store i64 %RBP_3, i64* %RBP
  store i64 %RIP_26, i64* %RIP
  store i64 %RSP_7, i64* %RSP
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  br label %exit_fn_4004C0
}

define void @fn_400680(%regset* noalias nocapture) {
entry_fn_400680:
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
  br label %bb_400680

exit_fn_400680:                                   ; preds = %bb_400680
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400680:                                        ; preds = %entry_fn_400680
  %RIP_1 = add i64 4195968, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RIP_2 = add i64 %RIP_1, 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %RSP_0 = load i64, i64* %RSP
  %RSP_1 = add i64 %RSP_0, 8
  %3 = inttoptr i64 %RSP_0 to i64*
  %RIP_3 = load i64, i64* %3
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  store i32 %EIP_2, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 %IP_2, i16* %IP
  store i64 %RIP_3, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br label %exit_fn_400680
}

define void @fn_400684(%regset* noalias nocapture) {
entry_fn_400684:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400684

exit_fn_400684:                                   ; preds = %bb_400684
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400684:                                        ; preds = %entry_fn_400684
  %RIP_1 = add i64 4195972, 4
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RSP_0 = load i64, i64* %RSP
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_2 = add i64 %RIP_1, 4
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %RIP_3 = add i64 %RIP_2, 1
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %5 = inttoptr i64 %RSP_2 to i64*
  %RIP_4 = load i64, i64* %5
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %ZF_0 = icmp eq i64 %RSP_2, 0
  %SF_0 = icmp slt i64 %RSP_2, 0
  %6 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_1, i64 8)
  %OF_0 = extractvalue { i64, i1 } %6, 1
  %7 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_1, i64 8)
  %CF_0 = extractvalue { i64, i1 } %7, 1
  %8 = trunc i64 %RSP_2 to i8
  %9 = call i8 @llvm.ctpop.i8(i8 %8)
  %10 = trunc i8 %9 to i1
  %PF_0 = icmp eq i1 %10, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %11 = zext i1 %CF_0 to i32
  %12 = shl i32 %11, 0
  %13 = or i32 %12, %CtlSysEFLAGS_0
  %14 = zext i1 %PF_0 to i32
  %15 = shl i32 %14, 2
  %16 = or i32 %15, %13
  %17 = zext i1 false to i32
  %18 = shl i32 %17, 4
  %19 = or i32 %18, %16
  %20 = zext i1 %ZF_0 to i32
  %21 = shl i32 %20, 6
  %22 = or i32 %21, %19
  %23 = zext i1 %SF_0 to i32
  %24 = shl i32 %23, 7
  %25 = or i32 %24, %22
  %26 = zext i1 %OF_0 to i32
  %27 = shl i32 %26, 11
  %EFLAGS_1 = or i32 %27, %25
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_3, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_3, i16* %IP
  store i64 %RIP_4, i64* %RIP
  store i64 %RSP_3, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br label %exit_fn_400684
}

define void @fn_400610(%regset* noalias nocapture) {
entry_fn_400610:
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
  %R15_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 76
  %R15_init = load i64, i64* %R15_ptr
  %R15 = alloca i64
  store i64 %R15_init, i64* %R15
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
  %R14_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 75
  %R14_init = load i64, i64* %R14_ptr
  %R14 = alloca i64
  store i64 %R14_init, i64* %R14
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %R15D_init = trunc i64 %R15_init to i32
  %R15D = alloca i32
  store i32 %R15D_init, i32* %R15D
  %R15W_init = trunc i64 %R15_init to i16
  %R15W = alloca i16
  store i16 %R15W_init, i16* %R15W
  %R15B_init = trunc i64 %R15_init to i8
  %R15B = alloca i8
  store i8 %R15B_init, i8* %R15B
  %R13_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 74
  %R13_init = load i64, i64* %R13_ptr
  %R13 = alloca i64
  store i64 %R13_init, i64* %R13
  %R12_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 73
  %R12_init = load i64, i64* %R12_ptr
  %R12 = alloca i64
  store i64 %R12_init, i64* %R12
  %R12D_init = trunc i64 %R12_init to i32
  %R12D = alloca i32
  store i32 %R12D_init, i32* %R12D
  %R12W_init = trunc i64 %R12_init to i16
  %R12W = alloca i16
  store i16 %R12W_init, i16* %R12W
  %R12B_init = trunc i64 %R12_init to i8
  %R12B = alloca i8
  store i8 %R12B_init, i8* %R12B
  %RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
  %RBP_init = load i64, i64* %RBP_ptr
  %RBP = alloca i64
  store i64 %RBP_init, i64* %RBP
  %EBP_init = trunc i64 %RBP_init to i32
  %EBP = alloca i32
  store i32 %EBP_init, i32* %EBP
  %BP_init = trunc i64 %RBP_init to i16
  %BP = alloca i16
  store i16 %BP_init, i16* %BP
  %BPL_init = trunc i64 %RBP_init to i8
  %BPL = alloca i8
  store i8 %BPL_init, i8* %BPL
  %RBX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 10
  %RBX_init = load i64, i64* %RBX_ptr
  %RBX = alloca i64
  store i64 %RBX_init, i64* %RBX
  %RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
  %RDI_init = load i64, i64* %RDI_ptr
  %RDI = alloca i64
  store i64 %RDI_init, i64* %RDI
  %EDI_init = trunc i64 %RDI_init to i32
  %EDI = alloca i32
  store i32 %EDI_init, i32* %EDI
  %R13D_init = trunc i64 %R13_init to i32
  %R13D = alloca i32
  store i32 %R13D_init, i32* %R13D
  %R13W_init = trunc i64 %R13_init to i16
  %R13W = alloca i16
  store i16 %R13W_init, i16* %R13W
  %R13B_init = trunc i64 %R13_init to i8
  %R13B = alloca i8
  store i8 %R13B_init, i8* %R13B
  %RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
  %RSI_init = load i64, i64* %RSI_ptr
  %RSI = alloca i64
  store i64 %RSI_init, i64* %RSI
  %R14D_init = trunc i64 %R14_init to i32
  %R14D = alloca i32
  store i32 %R14D_init, i32* %R14D
  %R14W_init = trunc i64 %R14_init to i16
  %R14W = alloca i16
  store i16 %R14W_init, i16* %R14W
  %R14B_init = trunc i64 %R14_init to i8
  %R14B = alloca i8
  store i8 %R14B_init, i8* %R14B
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  %EBX_init = trunc i64 %RBX_init to i32
  %EBX = alloca i32
  store i32 %EBX_init, i32* %EBX
  %BX_init = trunc i64 %RBX_init to i16
  %BX = alloca i16
  store i16 %BX_init, i16* %BX
  %BL_init = trunc i64 %RBX_init to i8
  %BL = alloca i8
  store i8 %BL_init, i8* %BL
  %1 = lshr i64 %RBX_init, 8
  %BH_init = trunc i64 %1 to i8
  %BH = alloca i8
  store i8 %BH_init, i8* %BH
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %2 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %2 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
  %ESI_init = trunc i64 %RSI_init to i32
  %ESI = alloca i32
  store i32 %ESI_init, i32* %ESI
  %SI_init = trunc i64 %RSI_init to i16
  %SI = alloca i16
  store i16 %SI_init, i16* %SI
  %SIL_init = trunc i64 %RSI_init to i8
  %SIL = alloca i8
  store i8 %SIL_init, i8* %SIL
  %DI_init = trunc i64 %RDI_init to i16
  %DI = alloca i16
  store i16 %DI_init, i16* %DI
  %DIL_init = trunc i64 %RDI_init to i8
  %DIL = alloca i8
  store i8 %DIL_init, i8* %DIL
  br label %bb_400610

exit_fn_400610:                                   ; preds = %bb_400666
  %3 = load i32, i32* %CtlSysEFLAGS
  store i32 %3, i32* %CtlSysEFLAGS_ptr
  %4 = load i32, i32* %EFLAGS
  store i32 %4, i32* %EFLAGS_ptr
  %5 = load i64, i64* %RBP
  store i64 %5, i64* %RBP_ptr
  %6 = load i64, i64* %RBX
  store i64 %6, i64* %RBX_ptr
  %7 = load i64, i64* %RDI
  store i64 %7, i64* %RDI_ptr
  %8 = load i64, i64* %RDX
  store i64 %8, i64* %RDX_ptr
  %9 = load i64, i64* %RIP
  store i64 %9, i64* %RIP_ptr
  %10 = load i64, i64* %RSI
  store i64 %10, i64* %RSI_ptr
  %11 = load i64, i64* %RSP
  store i64 %11, i64* %RSP_ptr
  %12 = load i64, i64* %R12
  store i64 %12, i64* %R12_ptr
  %13 = load i64, i64* %R13
  store i64 %13, i64* %R13_ptr
  %14 = load i64, i64* %R14
  store i64 %14, i64* %R14_ptr
  %15 = load i64, i64* %R15
  store i64 %15, i64* %R15_ptr
  ret void

bb_400610:                                        ; preds = %entry_fn_400610
  %RIP_1 = add i64 4195856, 2
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %R15_0 = load i64, i64* %R15
  %RSP_0 = load i64, i64* %RSP
  %16 = sub i64 %RSP_0, 8
  %17 = inttoptr i64 %16 to i64*
  store i64 %R15_0, i64* %17, align 1
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %RIP_2 = add i64 %RIP_1, 2
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %R14_0 = load i64, i64* %R14
  %18 = sub i64 %RSP_1, 8
  %19 = inttoptr i64 %18 to i64*
  store i64 %R14_0, i64* %19, align 1
  %RSP_2 = sub i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %RIP_3 = add i64 %RIP_2, 3
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RDX_0 = load i64, i64* %RDX
  %R15D_0 = trunc i64 %RDX_0 to i32
  %R15W_0 = trunc i64 %RDX_0 to i16
  %R15B_0 = trunc i64 %RDX_0 to i8
  %RIP_4 = add i64 %RIP_3, 2
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %R13_0 = load i64, i64* %R13
  %20 = sub i64 %RSP_2, 8
  %21 = inttoptr i64 %20 to i64*
  store i64 %R13_0, i64* %21, align 1
  %RSP_3 = sub i64 %RSP_2, 8
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %RIP_5 = add i64 %RIP_4, 2
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %R12_0 = load i64, i64* %R12
  %22 = sub i64 %RSP_3, 8
  %23 = inttoptr i64 %22 to i64*
  store i64 %R12_0, i64* %23, align 1
  %RSP_4 = sub i64 %RSP_3, 8
  %ESP_3 = trunc i64 %RSP_4 to i32
  %SP_3 = trunc i64 %RSP_4 to i16
  %SPL_3 = trunc i64 %RSP_4 to i8
  %RIP_6 = add i64 %RIP_5, 7
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %R12_1 = add i64 %RIP_6, 2099182
  %R12D_0 = trunc i64 %R12_1 to i32
  %R12W_0 = trunc i64 %R12_1 to i16
  %R12B_0 = trunc i64 %R12_1 to i8
  %RIP_7 = add i64 %RIP_6, 1
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RBP_0 = load i64, i64* %RBP
  %24 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %24 to i64*
  store i64 %RBP_0, i64* %25, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %RIP_8 = add i64 %RIP_7, 7
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RBP_1 = add i64 %RIP_8, 2099182
  %EBP_0 = trunc i64 %RBP_1 to i32
  %BP_0 = trunc i64 %RBP_1 to i16
  %BPL_0 = trunc i64 %RBP_1 to i8
  %RIP_9 = add i64 %RIP_8, 1
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %RBX_0 = load i64, i64* %RBX
  %26 = sub i64 %RSP_5, 8
  %27 = inttoptr i64 %26 to i64*
  store i64 %RBX_0, i64* %27, align 1
  %RSP_6 = sub i64 %RSP_5, 8
  %ESP_5 = trunc i64 %RSP_6 to i32
  %SP_5 = trunc i64 %RSP_6 to i16
  %SPL_5 = trunc i64 %RSP_6 to i8
  %RIP_10 = add i64 %RIP_9, 3
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %RDI_0 = load i64, i64* %RDI
  %EDI_0 = trunc i64 %RDI_0 to i32
  %R13_1 = zext i32 %EDI_0 to i64
  %R13W_0 = trunc i32 %EDI_0 to i16
  %R13B_0 = trunc i32 %EDI_0 to i8
  %RIP_11 = add i64 %RIP_10, 3
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %RSI_0 = load i64, i64* %RSI
  %R14D_0 = trunc i64 %RSI_0 to i32
  %R14W_0 = trunc i64 %RSI_0 to i16
  %R14B_0 = trunc i64 %RSI_0 to i8
  %RIP_12 = add i64 %RIP_11, 3
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %RBP_2 = sub i64 %RBP_1, %R12_1
  %EBP_1 = trunc i64 %RBP_2 to i32
  %BP_1 = trunc i64 %RBP_2 to i16
  %BPL_1 = trunc i64 %RBP_2 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_13 = add i64 %RIP_12, 4
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %RSP_7 = sub i64 %RSP_6, 8
  %ESP_6 = trunc i64 %RSP_7 to i32
  %SP_6 = trunc i64 %RSP_7 to i16
  %SPL_6 = trunc i64 %RSP_7 to i8
  %RIP_14 = add i64 %RIP_13, 4
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %28 = zext i8 3 to i64
  %RBP_3 = ashr i64 %RBP_2, %28
  %EBP_2 = trunc i64 %RBP_3 to i32
  %BP_2 = trunc i64 %RBP_3 to i16
  %BPL_2 = trunc i64 %RBP_3 to i8
  %RIP_15 = add i64 %RIP_14, 5
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %RSP_8 = sub i64 %RSP_7, 8
  %29 = inttoptr i64 %RSP_8 to i64*
  store i64 4195905, i64* %29
  %ESP_7 = trunc i64 %RSP_8 to i32
  %SP_7 = trunc i64 %RSP_8 to i16
  %SPL_7 = trunc i64 %RSP_8 to i8
  store i16 %BP_2, i16* %BP
  store i8 %BPL_2, i8* %BPL
  store i32 %EBP_2, i32* %EBP
  store i32 %EDI_0, i32* %EDI
  %ZF_0 = icmp eq i64 %RBP_3, 0
  %SF_0 = icmp slt i64 %RBP_3, 0
  %30 = trunc i64 %RBP_3 to i8
  %31 = call i8 @llvm.ctpop.i8(i8 %30)
  %32 = trunc i8 %31 to i1
  %PF_0 = icmp eq i1 %32, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %33 = zext i1 false to i32
  %34 = shl i32 %33, 0
  %35 = or i32 %34, %CtlSysEFLAGS_0
  %36 = zext i1 %PF_0 to i32
  %37 = shl i32 %36, 2
  %38 = or i32 %37, %35
  %39 = zext i1 false to i32
  %40 = shl i32 %39, 4
  %41 = or i32 %40, %38
  %42 = zext i1 %ZF_0 to i32
  %43 = shl i32 %42, 6
  %44 = or i32 %43, %41
  %45 = zext i1 %SF_0 to i32
  %46 = shl i32 %45, 7
  %47 = or i32 %46, %44
  %48 = zext i1 false to i32
  %49 = shl i32 %48, 11
  %EFLAGS_1 = or i32 %49, %47
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_14, i32* %EIP
  store i32 %ESP_7, i32* %ESP
  store i16 %IP_14, i16* %IP
  store i64 %RBP_3, i64* %RBP
  store i64 %RBX_0, i64* %RBX
  store i64 %RDI_0, i64* %RDI
  store i64 %RDX_0, i64* %RDX
  store i64 %RIP_15, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_8, i64* %RSP
  store i16 %SP_7, i16* %SP
  store i8 %SPL_7, i8* %SPL
  store i64 %R12_1, i64* %R12
  store i64 %R13_1, i64* %R13
  store i64 %RSI_0, i64* %R14
  store i64 %RDX_0, i64* %R15
  store i8 %R12B_0, i8* %R12B
  store i8 %R13B_0, i8* %R13B
  store i8 %R14B_0, i8* %R14B
  store i8 %R15B_0, i8* %R15B
  store i32 %R12D_0, i32* %R12D
  store i32 %EDI_0, i32* %R13D
  store i32 %R14D_0, i32* %R14D
  store i32 %R15D_0, i32* %R15D
  store i16 %R12W_0, i16* %R12W
  store i16 %R13W_0, i16* %R13W
  store i16 %R14W_0, i16* %R14W
  store i16 %R15W_0, i16* %R15W
  %50 = load i32, i32* %CtlSysEFLAGS
  store i32 %50, i32* %CtlSysEFLAGS_ptr
  %51 = load i32, i32* %EFLAGS
  store i32 %51, i32* %EFLAGS_ptr
  %52 = load i64, i64* %RBP
  store i64 %52, i64* %RBP_ptr
  %53 = load i64, i64* %RBX
  store i64 %53, i64* %RBX_ptr
  %54 = load i64, i64* %RDI
  store i64 %54, i64* %RDI_ptr
  %55 = load i64, i64* %RDX
  store i64 %55, i64* %RDX_ptr
  %56 = load i64, i64* %RIP
  store i64 %56, i64* %RIP_ptr
  %57 = load i64, i64* %RSI
  store i64 %57, i64* %RSI_ptr
  %58 = load i64, i64* %RSP
  store i64 %58, i64* %RSP_ptr
  %59 = load i64, i64* %R12
  store i64 %59, i64* %R12_ptr
  %60 = load i64, i64* %R13
  store i64 %60, i64* %R13_ptr
  %61 = load i64, i64* %R14
  store i64 %61, i64* %R14_ptr
  %62 = load i64, i64* %R15
  store i64 %62, i64* %R15_ptr
  call void @fn_4003A8(%regset* %0)
  %63 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %63, i32* %CtlSysEFLAGS
  %64 = load i32, i32* %EFLAGS_ptr
  store i32 %64, i32* %EFLAGS
  %65 = load i64, i64* %RBP_ptr
  store i64 %65, i64* %RBP
  %66 = load i64, i64* %RBX_ptr
  store i64 %66, i64* %RBX
  %67 = load i64, i64* %RDI_ptr
  store i64 %67, i64* %RDI
  %68 = load i64, i64* %RDX_ptr
  store i64 %68, i64* %RDX
  %69 = load i64, i64* %RIP_ptr
  store i64 %69, i64* %RIP
  %70 = load i64, i64* %RSI_ptr
  store i64 %70, i64* %RSI
  %71 = load i64, i64* %RSP_ptr
  store i64 %71, i64* %RSP
  %72 = load i64, i64* %R12_ptr
  store i64 %72, i64* %R12
  %73 = load i64, i64* %R13_ptr
  store i64 %73, i64* %R13
  %74 = load i64, i64* %R14_ptr
  store i64 %74, i64* %R14
  %75 = load i64, i64* %R15_ptr
  store i64 %75, i64* %R15
  %RIP_16 = load i64, i64* %RIP
  %RIP_17 = add i64 %RIP_16, 3
  %EIP_15 = trunc i64 %RIP_17 to i32
  %IP_15 = trunc i64 %RIP_17 to i16
  %RBP_4 = load i64, i64* %RBP
  %76 = and i64 %RBP_4, %RBP_4
  %CC_A_0 = icmp ugt i64 %76, 0
  %CC_AE_0 = icmp uge i64 %76, 0
  %CC_B_0 = icmp ult i64 %76, 0
  %CC_BE_0 = icmp ule i64 %76, 0
  %CC_L_0 = icmp slt i64 %76, 0
  %CC_LE_0 = icmp sle i64 %76, 0
  %CC_G_0 = icmp sgt i64 %76, 0
  %CC_GE_0 = icmp sge i64 %76, 0
  %CC_E_0 = icmp eq i64 %76, 0
  %CC_NE_0 = icmp ne i64 %76, 0
  %77 = sub i64 %76, 0
  %ZF_1 = icmp eq i64 %77, 0
  %SF_1 = icmp slt i64 %77, 0
  %78 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %76, i64 0)
  %OF_1 = extractvalue { i64, i1 } %78, 1
  %79 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %76, i64 0)
  %CF_1 = extractvalue { i64, i1 } %79, 1
  %80 = trunc i64 %77 to i8
  %81 = call i8 @llvm.ctpop.i8(i8 %80)
  %82 = trunc i8 %81 to i1
  %PF_1 = icmp eq i1 %82, false
  %83 = zext i1 %CF_1 to i32
  %84 = shl i32 %83, 0
  %85 = or i32 %84, %CtlSysEFLAGS_0
  %86 = zext i1 %PF_1 to i32
  %87 = shl i32 %86, 2
  %88 = or i32 %87, %85
  %89 = zext i1 false to i32
  %90 = shl i32 %89, 4
  %91 = or i32 %90, %88
  %92 = zext i1 %ZF_1 to i32
  %93 = shl i32 %92, 6
  %94 = or i32 %93, %91
  %95 = zext i1 %SF_1 to i32
  %96 = shl i32 %95, 7
  %97 = or i32 %96, %94
  %98 = zext i1 %OF_1 to i32
  %99 = shl i32 %98, 11
  %EFLAGS_2 = or i32 %99, %97
  %RIP_18 = add i64 %RIP_17, 2
  %EIP_16 = trunc i64 %RIP_18 to i32
  %IP_16 = trunc i64 %RIP_18 to i16
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EFLAGS_2, i32* %EFLAGS
  store i32 4195942, i32* %EIP
  store i16 1638, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195942, i64* %RIP
  br i1 %CC_E_0, label %bb_400666, label %bb_400646

bb_400646:                                        ; preds = %bb_400610
  %RIP_21 = add i64 4195910, 2
  %EIP_18 = trunc i64 %RIP_21 to i32
  %IP_18 = trunc i64 %RIP_21 to i16
  %RBX_1 = load i64, i64* %RBX
  %EBX_0 = trunc i64 %RBX_1 to i32
  %EBX_1 = xor i32 %EBX_0, %EBX_0
  %RBX_2 = zext i32 %EBX_1 to i64
  %BX_0 = trunc i32 %EBX_1 to i16
  %BL_0 = trunc i32 %EBX_1 to i8
  %100 = lshr i32 %EBX_1, 8
  %BH_0 = trunc i32 %100 to i8
  %EFLAGS_3 = load i32, i32* %EFLAGS
  %RIP_22 = add i64 %RIP_21, 8
  %EIP_19 = trunc i64 %RIP_22 to i32
  %IP_19 = trunc i64 %RIP_22 to i16
  %ZF_01 = icmp eq i32 %EBX_1, 0
  %SF_02 = icmp slt i32 %EBX_1, 0
  %101 = trunc i32 %EBX_1 to i8
  %102 = call i8 @llvm.ctpop.i8(i8 %101)
  %103 = trunc i8 %102 to i1
  %PF_03 = icmp eq i1 %103, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %104 = zext i1 false to i32
  %105 = shl i32 %104, 0
  %106 = or i32 %105, %CtlSysEFLAGS_1
  %107 = zext i1 %PF_03 to i32
  %108 = shl i32 %107, 2
  %109 = or i32 %108, %106
  %110 = zext i1 false to i32
  %111 = shl i32 %110, 4
  %112 = or i32 %111, %109
  %113 = zext i1 %ZF_01 to i32
  %114 = shl i32 %113, 6
  %115 = or i32 %114, %112
  %116 = zext i1 %SF_02 to i32
  %117 = shl i32 %116, 7
  %118 = or i32 %117, %115
  %119 = zext i1 false to i32
  %120 = shl i32 %119, 11
  %EFLAGS_4 = or i32 %120, %118
  store i8 %BH_0, i8* %BH
  store i8 %BL_0, i8* %BL
  store i16 %BX_0, i16* %BX
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 %EBX_1, i32* %EBX
  store i32 %EFLAGS_4, i32* %EFLAGS
  store i32 %EIP_19, i32* %EIP
  store i16 %IP_19, i16* %IP
  store i64 %RBX_2, i64* %RBX
  store i64 %RIP_22, i64* %RIP
  br label %bb_400650

bb_400650:                                        ; preds = %bb_400650, %bb_400646
  %RIP_34 = add i64 4195920, 3
  %EIP_29 = trunc i64 %RIP_34 to i32
  %IP_29 = trunc i64 %RIP_34 to i16
  %R15_2 = load i64, i64* %R15
  %EDX_0 = trunc i64 %R15_2 to i32
  %DX_0 = trunc i64 %R15_2 to i16
  %DL_0 = trunc i64 %R15_2 to i8
  %121 = lshr i64 %R15_2, 8
  %DH_0 = trunc i64 %121 to i8
  %RIP_35 = add i64 %RIP_34, 3
  %EIP_30 = trunc i64 %RIP_35 to i32
  %IP_30 = trunc i64 %RIP_35 to i16
  %R14_2 = load i64, i64* %R14
  %ESI_0 = trunc i64 %R14_2 to i32
  %SI_0 = trunc i64 %R14_2 to i16
  %SIL_0 = trunc i64 %R14_2 to i8
  %RIP_36 = add i64 %RIP_35, 3
  %EIP_31 = trunc i64 %RIP_36 to i32
  %IP_31 = trunc i64 %RIP_36 to i16
  %R13_3 = load i64, i64* %R13
  %R13D_1 = trunc i64 %R13_3 to i32
  %RDI_1 = load i64, i64* %RDI
  %RDI_2 = zext i32 %R13D_1 to i64
  %DI_0 = trunc i32 %R13D_1 to i16
  %DIL_0 = trunc i32 %R13D_1 to i8
  %RIP_37 = add i64 %RIP_36, 4
  %EIP_32 = trunc i64 %RIP_37 to i32
  %IP_32 = trunc i64 %RIP_37 to i16
  %R12_3 = load i64, i64* %R12
  %RBX_4 = load i64, i64* %RBX
  %122 = mul i64 %RBX_4, 8
  %123 = add i64 %R12_3, %122
  %124 = inttoptr i64 %123 to i64*
  %125 = load i64, i64* %124, align 1
  %RSP_18 = load i64, i64* %RSP
  %RSP_19 = sub i64 %RSP_18, 8
  %126 = inttoptr i64 %RSP_19 to i64*
  store i64 4195933, i64* %126
  %ESP_16 = trunc i64 %RSP_19 to i32
  %SP_16 = trunc i64 %RSP_19 to i16
  %SPL_16 = trunc i64 %RSP_19 to i8
  %127 = inttoptr i64 %125 to i8*
  %128 = call i8* @llvm.dc.translate.at(i8* %127)
  %129 = bitcast i8* %128 to void (%regset*)*
  store i8 %DH_0, i8* %DH
  store i16 %DI_0, i16* %DI
  store i8 %DIL_0, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %R13D_1, i32* %EDI
  store i32 %EDX_0, i32* %EDX
  store i32 %EIP_32, i32* %EIP
  store i32 %ESI_0, i32* %ESI
  store i32 %ESP_16, i32* %ESP
  store i16 %IP_32, i16* %IP
  store i64 %RBX_4, i64* %RBX
  store i64 %RDI_2, i64* %RDI
  store i64 %R15_2, i64* %RDX
  store i64 %RIP_37, i64* %RIP
  store i64 %R14_2, i64* %RSI
  store i64 %RSP_19, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_16, i16* %SP
  store i8 %SPL_16, i8* %SPL
  store i64 %R12_3, i64* %R12
  store i64 %R13_3, i64* %R13
  store i64 %R14_2, i64* %R14
  store i64 %R15_2, i64* %R15
  store i32 %R13D_1, i32* %R13D
  %130 = load i32, i32* %CtlSysEFLAGS
  store i32 %130, i32* %CtlSysEFLAGS_ptr
  %131 = load i32, i32* %EFLAGS
  store i32 %131, i32* %EFLAGS_ptr
  %132 = load i64, i64* %RBP
  store i64 %132, i64* %RBP_ptr
  %133 = load i64, i64* %RBX
  store i64 %133, i64* %RBX_ptr
  %134 = load i64, i64* %RDI
  store i64 %134, i64* %RDI_ptr
  %135 = load i64, i64* %RDX
  store i64 %135, i64* %RDX_ptr
  %136 = load i64, i64* %RIP
  store i64 %136, i64* %RIP_ptr
  %137 = load i64, i64* %RSI
  store i64 %137, i64* %RSI_ptr
  %138 = load i64, i64* %RSP
  store i64 %138, i64* %RSP_ptr
  %139 = load i64, i64* %R12
  store i64 %139, i64* %R12_ptr
  %140 = load i64, i64* %R13
  store i64 %140, i64* %R13_ptr
  %141 = load i64, i64* %R14
  store i64 %141, i64* %R14_ptr
  %142 = load i64, i64* %R15
  store i64 %142, i64* %R15_ptr
  call void %129(%regset* %0)
  %143 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %143, i32* %CtlSysEFLAGS
  %144 = load i32, i32* %EFLAGS_ptr
  store i32 %144, i32* %EFLAGS
  %145 = load i64, i64* %RBP_ptr
  store i64 %145, i64* %RBP
  %146 = load i64, i64* %RBX_ptr
  store i64 %146, i64* %RBX
  %147 = load i64, i64* %RDI_ptr
  store i64 %147, i64* %RDI
  %148 = load i64, i64* %RDX_ptr
  store i64 %148, i64* %RDX
  %149 = load i64, i64* %RIP_ptr
  store i64 %149, i64* %RIP
  %150 = load i64, i64* %RSI_ptr
  store i64 %150, i64* %RSI
  %151 = load i64, i64* %RSP_ptr
  store i64 %151, i64* %RSP
  %152 = load i64, i64* %R12_ptr
  store i64 %152, i64* %R12
  %153 = load i64, i64* %R13_ptr
  store i64 %153, i64* %R13
  %154 = load i64, i64* %R14_ptr
  store i64 %154, i64* %R14
  %155 = load i64, i64* %R15_ptr
  store i64 %155, i64* %R15
  %RIP_38 = load i64, i64* %RIP
  %RIP_39 = add i64 %RIP_38, 4
  %EIP_33 = trunc i64 %RIP_39 to i32
  %IP_33 = trunc i64 %RIP_39 to i16
  %RBX_5 = load i64, i64* %RBX
  %RBX_6 = add i64 %RBX_5, 1
  %EBX_3 = trunc i64 %RBX_6 to i32
  %BX_2 = trunc i64 %RBX_6 to i16
  %BL_2 = trunc i64 %RBX_6 to i8
  %156 = lshr i64 %RBX_6, 8
  %BH_2 = trunc i64 %156 to i8
  %EFLAGS_7 = load i32, i32* %EFLAGS
  %RIP_40 = add i64 %RIP_39, 3
  %EIP_34 = trunc i64 %RIP_40 to i32
  %IP_34 = trunc i64 %RIP_40 to i16
  %RBP_6 = load i64, i64* %RBP
  %CC_A_07 = icmp ugt i64 %RBP_6, %RBX_6
  %CC_AE_08 = icmp uge i64 %RBP_6, %RBX_6
  %CC_B_09 = icmp ult i64 %RBP_6, %RBX_6
  %CC_BE_010 = icmp ule i64 %RBP_6, %RBX_6
  %CC_L_011 = icmp slt i64 %RBP_6, %RBX_6
  %CC_LE_012 = icmp sle i64 %RBP_6, %RBX_6
  %CC_G_013 = icmp sgt i64 %RBP_6, %RBX_6
  %CC_GE_014 = icmp sge i64 %RBP_6, %RBX_6
  %CC_E_015 = icmp eq i64 %RBP_6, %RBX_6
  %CC_NE_016 = icmp ne i64 %RBP_6, %RBX_6
  %157 = sub i64 %RBP_6, %RBX_6
  %ZF_017 = icmp eq i64 %157, 0
  %SF_018 = icmp slt i64 %157, 0
  %158 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RBP_6, i64 %RBX_6)
  %OF_019 = extractvalue { i64, i1 } %158, 1
  %159 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RBP_6, i64 %RBX_6)
  %CF_020 = extractvalue { i64, i1 } %159, 1
  %160 = trunc i64 %157 to i8
  %161 = call i8 @llvm.ctpop.i8(i8 %160)
  %162 = trunc i8 %161 to i1
  %PF_021 = icmp eq i1 %162, false
  %CtlSysEFLAGS_3 = load i32, i32* %CtlSysEFLAGS
  %163 = zext i1 %CF_020 to i32
  %164 = shl i32 %163, 0
  %165 = or i32 %164, %CtlSysEFLAGS_3
  %166 = zext i1 %PF_021 to i32
  %167 = shl i32 %166, 2
  %168 = or i32 %167, %165
  %169 = zext i1 false to i32
  %170 = shl i32 %169, 4
  %171 = or i32 %170, %168
  %172 = zext i1 %ZF_017 to i32
  %173 = shl i32 %172, 6
  %174 = or i32 %173, %171
  %175 = zext i1 %SF_018 to i32
  %176 = shl i32 %175, 7
  %177 = or i32 %176, %174
  %178 = zext i1 %OF_019 to i32
  %179 = shl i32 %178, 11
  %EFLAGS_8 = or i32 %179, %177
  %RIP_41 = add i64 %RIP_40, 2
  %EIP_35 = trunc i64 %RIP_41 to i32
  %IP_35 = trunc i64 %RIP_41 to i16
  store i8 %BH_2, i8* %BH
  store i8 %BL_2, i8* %BL
  store i16 %BX_2, i16* %BX
  store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
  store i32 %EBX_3, i32* %EBX
  store i32 %EFLAGS_8, i32* %EFLAGS
  store i32 4195920, i32* %EIP
  store i16 1616, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195920, i64* %RIP
  br i1 %CC_NE_016, label %bb_400650, label %bb_400666

bb_400666:                                        ; preds = %bb_400650, %bb_400610
  %RIP_24 = add i64 4195942, 4
  %EIP_20 = trunc i64 %RIP_24 to i32
  %IP_20 = trunc i64 %RIP_24 to i16
  %RSP_9 = load i64, i64* %RSP
  %RSP_10 = add i64 %RSP_9, 8
  %ESP_8 = trunc i64 %RSP_10 to i32
  %SP_8 = trunc i64 %RSP_10 to i16
  %SPL_8 = trunc i64 %RSP_10 to i8
  %EFLAGS_5 = load i32, i32* %EFLAGS
  %RIP_25 = add i64 %RIP_24, 1
  %EIP_21 = trunc i64 %RIP_25 to i32
  %IP_21 = trunc i64 %RIP_25 to i16
  %RSP_11 = add i64 %RSP_10, 8
  %ESP_9 = trunc i64 %RSP_11 to i32
  %SP_9 = trunc i64 %RSP_11 to i16
  %SPL_9 = trunc i64 %RSP_11 to i8
  %180 = sub i64 %RSP_11, 8
  %181 = inttoptr i64 %180 to i64*
  %RBX_3 = load i64, i64* %181, align 1
  %EBX_2 = trunc i64 %RBX_3 to i32
  %BX_1 = trunc i64 %RBX_3 to i16
  %BL_1 = trunc i64 %RBX_3 to i8
  %182 = lshr i64 %RBX_3, 8
  %BH_1 = trunc i64 %182 to i8
  %RIP_26 = add i64 %RIP_25, 1
  %EIP_22 = trunc i64 %RIP_26 to i32
  %IP_22 = trunc i64 %RIP_26 to i16
  %RSP_12 = add i64 %RSP_11, 8
  %ESP_10 = trunc i64 %RSP_12 to i32
  %SP_10 = trunc i64 %RSP_12 to i16
  %SPL_10 = trunc i64 %RSP_12 to i8
  %183 = sub i64 %RSP_12, 8
  %184 = inttoptr i64 %183 to i64*
  %RBP_5 = load i64, i64* %184, align 1
  %EBP_3 = trunc i64 %RBP_5 to i32
  %BP_3 = trunc i64 %RBP_5 to i16
  %BPL_3 = trunc i64 %RBP_5 to i8
  %RIP_27 = add i64 %RIP_26, 2
  %EIP_23 = trunc i64 %RIP_27 to i32
  %IP_23 = trunc i64 %RIP_27 to i16
  %RSP_13 = add i64 %RSP_12, 8
  %ESP_11 = trunc i64 %RSP_13 to i32
  %SP_11 = trunc i64 %RSP_13 to i16
  %SPL_11 = trunc i64 %RSP_13 to i8
  %185 = sub i64 %RSP_13, 8
  %186 = inttoptr i64 %185 to i64*
  %R12_2 = load i64, i64* %186, align 1
  %R12D_1 = trunc i64 %R12_2 to i32
  %R12W_1 = trunc i64 %R12_2 to i16
  %R12B_1 = trunc i64 %R12_2 to i8
  %RIP_28 = add i64 %RIP_27, 2
  %EIP_24 = trunc i64 %RIP_28 to i32
  %IP_24 = trunc i64 %RIP_28 to i16
  %RSP_14 = add i64 %RSP_13, 8
  %ESP_12 = trunc i64 %RSP_14 to i32
  %SP_12 = trunc i64 %RSP_14 to i16
  %SPL_12 = trunc i64 %RSP_14 to i8
  %187 = sub i64 %RSP_14, 8
  %188 = inttoptr i64 %187 to i64*
  %R13_2 = load i64, i64* %188, align 1
  %R13D_0 = trunc i64 %R13_2 to i32
  %R13W_1 = trunc i64 %R13_2 to i16
  %R13B_1 = trunc i64 %R13_2 to i8
  %RIP_29 = add i64 %RIP_28, 2
  %EIP_25 = trunc i64 %RIP_29 to i32
  %IP_25 = trunc i64 %RIP_29 to i16
  %RSP_15 = add i64 %RSP_14, 8
  %ESP_13 = trunc i64 %RSP_15 to i32
  %SP_13 = trunc i64 %RSP_15 to i16
  %SPL_13 = trunc i64 %RSP_15 to i8
  %189 = sub i64 %RSP_15, 8
  %190 = inttoptr i64 %189 to i64*
  %R14_1 = load i64, i64* %190, align 1
  %R14D_1 = trunc i64 %R14_1 to i32
  %R14W_1 = trunc i64 %R14_1 to i16
  %R14B_1 = trunc i64 %R14_1 to i8
  %RIP_30 = add i64 %RIP_29, 2
  %EIP_26 = trunc i64 %RIP_30 to i32
  %IP_26 = trunc i64 %RIP_30 to i16
  %RSP_16 = add i64 %RSP_15, 8
  %ESP_14 = trunc i64 %RSP_16 to i32
  %SP_14 = trunc i64 %RSP_16 to i16
  %SPL_14 = trunc i64 %RSP_16 to i8
  %191 = sub i64 %RSP_16, 8
  %192 = inttoptr i64 %191 to i64*
  %R15_1 = load i64, i64* %192, align 1
  %R15D_1 = trunc i64 %R15_1 to i32
  %R15W_1 = trunc i64 %R15_1 to i16
  %R15B_1 = trunc i64 %R15_1 to i8
  %RIP_31 = add i64 %RIP_30, 1
  %EIP_27 = trunc i64 %RIP_31 to i32
  %IP_27 = trunc i64 %RIP_31 to i16
  %RSP_17 = add i64 %RSP_16, 8
  %193 = inttoptr i64 %RSP_16 to i64*
  %RIP_32 = load i64, i64* %193
  %ESP_15 = trunc i64 %RSP_17 to i32
  %SP_15 = trunc i64 %RSP_17 to i16
  %SPL_15 = trunc i64 %RSP_17 to i8
  %EIP_28 = trunc i64 %RIP_32 to i32
  %IP_28 = trunc i64 %RIP_32 to i16
  %ZF_04 = icmp eq i64 %RSP_10, 0
  %SF_05 = icmp slt i64 %RSP_10, 0
  %194 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_9, i64 8)
  %OF_0 = extractvalue { i64, i1 } %194, 1
  %195 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_9, i64 8)
  %CF_0 = extractvalue { i64, i1 } %195, 1
  %196 = trunc i64 %RSP_10 to i8
  %197 = call i8 @llvm.ctpop.i8(i8 %196)
  %198 = trunc i8 %197 to i1
  %PF_06 = icmp eq i1 %198, false
  %CtlSysEFLAGS_2 = load i32, i32* %CtlSysEFLAGS
  %199 = zext i1 %CF_0 to i32
  %200 = shl i32 %199, 0
  %201 = or i32 %200, %CtlSysEFLAGS_2
  %202 = zext i1 %PF_06 to i32
  %203 = shl i32 %202, 2
  %204 = or i32 %203, %201
  %205 = zext i1 false to i32
  %206 = shl i32 %205, 4
  %207 = or i32 %206, %204
  %208 = zext i1 %ZF_04 to i32
  %209 = shl i32 %208, 6
  %210 = or i32 %209, %207
  %211 = zext i1 %SF_05 to i32
  %212 = shl i32 %211, 7
  %213 = or i32 %212, %210
  %214 = zext i1 %OF_0 to i32
  %215 = shl i32 %214, 11
  %EFLAGS_6 = or i32 %215, %213
  store i8 %BH_1, i8* %BH
  store i8 %BL_1, i8* %BL
  store i16 %BP_3, i16* %BP
  store i8 %BPL_3, i8* %BPL
  store i16 %BX_1, i16* %BX
  store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
  store i32 %EBP_3, i32* %EBP
  store i32 %EBX_2, i32* %EBX
  store i32 %EFLAGS_6, i32* %EFLAGS
  store i32 %EIP_28, i32* %EIP
  store i32 %ESP_15, i32* %ESP
  store i16 %IP_28, i16* %IP
  store i64 %RBP_5, i64* %RBP
  store i64 %RBX_3, i64* %RBX
  store i64 %RIP_32, i64* %RIP
  store i64 %RSP_17, i64* %RSP
  store i16 %SP_15, i16* %SP
  store i8 %SPL_15, i8* %SPL
  store i64 %R12_2, i64* %R12
  store i64 %R13_2, i64* %R13
  store i64 %R14_1, i64* %R14
  store i64 %R15_1, i64* %R15
  store i8 %R12B_1, i8* %R12B
  store i8 %R13B_1, i8* %R13B
  store i8 %R14B_1, i8* %R14B
  store i8 %R15B_1, i8* %R15B
  store i32 %R12D_1, i32* %R12D
  store i32 %R13D_0, i32* %R13D
  store i32 %R14D_1, i32* %R14D
  store i32 %R15D_1, i32* %R15D
  store i16 %R12W_1, i16* %R12W
  store i16 %R13W_1, i16* %R13W
  store i16 %R14W_1, i16* %R14W
  store i16 %R15W_1, i16* %R15W
  br label %exit_fn_400610
}

define void @fn_4003A8(%regset* noalias nocapture) {
entry_fn_4003A8:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %AX_init = trunc i64 %RAX_init to i16
  %AX = alloca i16
  store i16 %AX_init, i16* %AX
  %AL_init = trunc i64 %RAX_init to i8
  %AL = alloca i8
  store i8 %AL_init, i8* %AL
  %1 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %1 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_4003A8

exit_fn_4003A8:                                   ; preds = %bb_4003BA
  %2 = load i32, i32* %CtlSysEFLAGS
  store i32 %2, i32* %CtlSysEFLAGS_ptr
  %3 = load i32, i32* %EFLAGS
  store i32 %3, i32* %EFLAGS_ptr
  %4 = load i64, i64* %RAX
  store i64 %4, i64* %RAX_ptr
  %5 = load i64, i64* %RIP
  store i64 %5, i64* %RIP_ptr
  %6 = load i64, i64* %RSP
  store i64 %6, i64* %RSP_ptr
  ret void

bb_4003A8:                                        ; preds = %entry_fn_4003A8
  %RIP_1 = add i64 4195240, 4
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RSP_0 = load i64, i64* %RSP
  %RSP_1 = sub i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_2 = add i64 %RIP_1, 7
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %7 = add i64 %RIP_2, 2100293
  %8 = inttoptr i64 %7 to i64*
  %RAX_0 = load i64, i64* %8, align 1
  %EAX_0 = trunc i64 %RAX_0 to i32
  %AX_0 = trunc i64 %RAX_0 to i16
  %AL_0 = trunc i64 %RAX_0 to i8
  %9 = lshr i64 %RAX_0, 8
  %AH_0 = trunc i64 %9 to i8
  %RIP_3 = add i64 %RIP_2, 3
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %10 = and i64 %RAX_0, %RAX_0
  %CC_A_0 = icmp ugt i64 %10, 0
  %CC_AE_0 = icmp uge i64 %10, 0
  %CC_B_0 = icmp ult i64 %10, 0
  %CC_BE_0 = icmp ule i64 %10, 0
  %CC_L_0 = icmp slt i64 %10, 0
  %CC_LE_0 = icmp sle i64 %10, 0
  %CC_G_0 = icmp sgt i64 %10, 0
  %CC_GE_0 = icmp sge i64 %10, 0
  %CC_E_0 = icmp eq i64 %10, 0
  %CC_NE_0 = icmp ne i64 %10, 0
  %11 = sub i64 %10, 0
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %10, i64 0)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %10, i64 0)
  %CF_0 = extractvalue { i64, i1 } %13, 1
  %14 = trunc i64 %11 to i8
  %15 = call i8 @llvm.ctpop.i8(i8 %14)
  %16 = trunc i8 %15 to i1
  %PF_0 = icmp eq i1 %16, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %17 = zext i1 %CF_0 to i32
  %18 = shl i32 %17, 0
  %19 = or i32 %18, %CtlSysEFLAGS_0
  %20 = zext i1 %PF_0 to i32
  %21 = shl i32 %20, 2
  %22 = or i32 %21, %19
  %23 = zext i1 false to i32
  %24 = shl i32 %23, 4
  %25 = or i32 %24, %22
  %26 = zext i1 %ZF_0 to i32
  %27 = shl i32 %26, 6
  %28 = or i32 %27, %25
  %29 = zext i1 %SF_0 to i32
  %30 = shl i32 %29, 7
  %31 = or i32 %30, %28
  %32 = zext i1 %OF_0 to i32
  %33 = shl i32 %32, 11
  %EFLAGS_1 = or i32 %33, %31
  %RIP_4 = add i64 %RIP_3, 2
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EAX_0, i32* %EAX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 4195258, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 954, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195258, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003BA, label %bb_4003B8

bb_4003B8:                                        ; preds = %bb_4003A8
  %RIP_7 = add i64 4195256, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195258, i64* %34
  %ESP_1 = trunc i64 %RSP_3 to i32
  %SP_1 = trunc i64 %RSP_3 to i16
  %SPL_1 = trunc i64 %RSP_3 to i8
  %35 = inttoptr i64 %RAX_1 to i8*
  %36 = call i8* @llvm.dc.translate.at(i8* %35)
  %37 = bitcast i8* %36 to void (%regset*)*
  store i32 %EIP_5, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 %IP_5, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RIP_7, i64* %RIP
  store i64 %RSP_3, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  %38 = load i32, i32* %CtlSysEFLAGS
  store i32 %38, i32* %CtlSysEFLAGS_ptr
  %39 = load i32, i32* %EFLAGS
  store i32 %39, i32* %EFLAGS_ptr
  %40 = load i64, i64* %RAX
  store i64 %40, i64* %RAX_ptr
  %41 = load i64, i64* %RIP
  store i64 %41, i64* %RIP_ptr
  %42 = load i64, i64* %RSP
  store i64 %42, i64* %RSP_ptr
  call void %37(%regset* %0)
  %43 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %43, i32* %CtlSysEFLAGS
  %44 = load i32, i32* %EFLAGS_ptr
  store i32 %44, i32* %EFLAGS
  %45 = load i64, i64* %RAX_ptr
  store i64 %45, i64* %RAX
  %46 = load i64, i64* %RIP_ptr
  store i64 %46, i64* %RIP
  %47 = load i64, i64* %RSP_ptr
  store i64 %47, i64* %RSP
  br label %bb_4003BA

bb_4003BA:                                        ; preds = %bb_4003B8, %bb_4003A8
  %RIP_9 = add i64 4195258, 4
  %EIP_6 = trunc i64 %RIP_9 to i32
  %IP_6 = trunc i64 %RIP_9 to i16
  %RSP_4 = load i64, i64* %RSP
  %RSP_5 = add i64 %RSP_4, 8
  %ESP_2 = trunc i64 %RSP_5 to i32
  %SP_2 = trunc i64 %RSP_5 to i16
  %SPL_2 = trunc i64 %RSP_5 to i8
  %EFLAGS_2 = load i32, i32* %EFLAGS
  %RIP_10 = add i64 %RIP_9, 1
  %EIP_7 = trunc i64 %RIP_10 to i32
  %IP_7 = trunc i64 %RIP_10 to i16
  %RSP_6 = add i64 %RSP_5, 8
  %48 = inttoptr i64 %RSP_5 to i64*
  %RIP_11 = load i64, i64* %48
  %ESP_3 = trunc i64 %RSP_6 to i32
  %SP_3 = trunc i64 %RSP_6 to i16
  %SPL_3 = trunc i64 %RSP_6 to i8
  %EIP_8 = trunc i64 %RIP_11 to i32
  %IP_8 = trunc i64 %RIP_11 to i16
  %ZF_01 = icmp eq i64 %RSP_5, 0
  %SF_02 = icmp slt i64 %RSP_5, 0
  %49 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_4, i64 8)
  %OF_03 = extractvalue { i64, i1 } %49, 1
  %50 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_4, i64 8)
  %CF_04 = extractvalue { i64, i1 } %50, 1
  %51 = trunc i64 %RSP_5 to i8
  %52 = call i8 @llvm.ctpop.i8(i8 %51)
  %53 = trunc i8 %52 to i1
  %PF_05 = icmp eq i1 %53, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %54 = zext i1 %CF_04 to i32
  %55 = shl i32 %54, 0
  %56 = or i32 %55, %CtlSysEFLAGS_1
  %57 = zext i1 %PF_05 to i32
  %58 = shl i32 %57, 2
  %59 = or i32 %58, %56
  %60 = zext i1 false to i32
  %61 = shl i32 %60, 4
  %62 = or i32 %61, %59
  %63 = zext i1 %ZF_01 to i32
  %64 = shl i32 %63, 6
  %65 = or i32 %64, %62
  %66 = zext i1 %SF_02 to i32
  %67 = shl i32 %66, 7
  %68 = or i32 %67, %65
  %69 = zext i1 %OF_03 to i32
  %70 = shl i32 %69, 11
  %EFLAGS_3 = or i32 %70, %68
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 %EFLAGS_3, i32* %EFLAGS
  store i32 %EIP_8, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_8, i16* %IP
  store i64 %RIP_11, i64* %RIP
  store i64 %RSP_6, i64* %RSP
  store i16 %SP_3, i16* %SP
  store i8 %SPL_3, i8* %SPL
  br label %exit_fn_4003A8
}

define void @fn_400410(%regset* noalias nocapture) {
entry_fn_400410:
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
  br label %bb_400410

exit_fn_400410:                                   ; preds = %bb_400410
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400410:                                        ; preds = %entry_fn_400410
  %RIP_1 = add i64 4195344, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RIP_2 = add i64 %RIP_1, 1
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %RSP_0 = load i64, i64* %RSP
  %RSP_1 = add i64 %RSP_0, 8
  %3 = inttoptr i64 %RSP_0 to i64*
  %RIP_3 = load i64, i64* %3
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  store i32 %EIP_2, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 %IP_2, i16* %IP
  store i64 %RIP_3, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br label %exit_fn_400410
}

define void @fn_4003E0(%regset* noalias nocapture) {
entry_fn_4003E0:
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %R9_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 70
  %R9_init = load i64, i64* %R9_ptr
  %R9 = alloca i64
  store i64 %R9_init, i64* %R9
  %R9D_init = trunc i64 %R9_init to i32
  %R9D = alloca i32
  store i32 %R9D_init, i32* %R9D
  %R9W_init = trunc i64 %R9_init to i16
  %R9W = alloca i16
  store i16 %R9W_init, i16* %R9W
  %R9B_init = trunc i64 %R9_init to i8
  %R9B = alloca i8
  store i8 %R9B_init, i8* %R9B
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
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %1 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %1 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %R8_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 69
  %R8_init = load i64, i64* %R8_ptr
  %R8 = alloca i64
  store i64 %R8_init, i64* %R8
  %R8D_init = trunc i64 %R8_init to i32
  %R8D = alloca i32
  store i32 %R8D_init, i32* %R8D
  %R8W_init = trunc i64 %R8_init to i16
  %R8W = alloca i16
  store i16 %R8W_init, i16* %R8W
  %R8B_init = trunc i64 %R8_init to i8
  %R8B = alloca i8
  store i8 %R8B_init, i8* %R8B
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
  %2 = lshr i64 %RCX_init, 8
  %CH_init = trunc i64 %2 to i8
  %CH = alloca i8
  store i8 %CH_init, i8* %CH
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
  br label %bb_4003E0

exit_fn_4003E0:                                   ; preds = %bb_4003E0
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
  %9 = load i64, i64* %RDX
  store i64 %9, i64* %RDX_ptr
  %10 = load i64, i64* %RIP
  store i64 %10, i64* %RIP_ptr
  %11 = load i64, i64* %RSI
  store i64 %11, i64* %RSI_ptr
  %12 = load i64, i64* %RSP
  store i64 %12, i64* %RSP_ptr
  %13 = load i64, i64* %R8
  store i64 %13, i64* %R8_ptr
  %14 = load i64, i64* %R9
  store i64 %14, i64* %R9_ptr
  ret void

bb_4003E0:                                        ; preds = %entry_fn_4003E0
  %RIP_1 = add i64 4195296, 2
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %EBP_0 = trunc i64 %RBP_0 to i32
  %EBP_1 = xor i32 %EBP_0, %EBP_0
  %RBP_1 = zext i32 %EBP_1 to i64
  %BP_0 = trunc i32 %EBP_1 to i16
  %BPL_0 = trunc i32 %EBP_1 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_2 = add i64 %RIP_1, 3
  %EIP_1 = trunc i64 %RIP_2 to i32
  %IP_1 = trunc i64 %RIP_2 to i16
  %RDX_0 = load i64, i64* %RDX
  %R9D_0 = trunc i64 %RDX_0 to i32
  %R9W_0 = trunc i64 %RDX_0 to i16
  %R9B_0 = trunc i64 %RDX_0 to i8
  %RIP_3 = add i64 %RIP_2, 1
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RSP_0 = load i64, i64* %RSP
  %RSP_1 = add i64 %RSP_0, 8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %SP_0 = trunc i64 %RSP_1 to i16
  %SPL_0 = trunc i64 %RSP_1 to i8
  %15 = sub i64 %RSP_1, 8
  %16 = inttoptr i64 %15 to i64*
  %RSI_0 = load i64, i64* %16, align 1
  %ESI_0 = trunc i64 %RSI_0 to i32
  %SI_0 = trunc i64 %RSI_0 to i16
  %SIL_0 = trunc i64 %RSI_0 to i8
  %RIP_4 = add i64 %RIP_3, 3
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %EDX_0 = trunc i64 %RSP_1 to i32
  %DX_0 = trunc i64 %RSP_1 to i16
  %DL_0 = trunc i64 %RSP_1 to i8
  %17 = lshr i64 %RSP_1, 8
  %DH_0 = trunc i64 %17 to i8
  %RIP_5 = add i64 %RIP_4, 4
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %RSP_2 = and i64 %RSP_1, -16
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %RIP_6 = add i64 %RIP_5, 1
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RAX_0 = load i64, i64* %RAX
  %18 = sub i64 %RSP_2, 8
  %19 = inttoptr i64 %18 to i64*
  store i64 %RAX_0, i64* %19, align 1
  %RSP_3 = sub i64 %RSP_2, 8
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %RIP_7 = add i64 %RIP_6, 1
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %20 = sub i64 %RSP_3, 8
  %21 = inttoptr i64 %20 to i64*
  store i64 %RSP_3, i64* %21, align 1
  %RSP_4 = sub i64 %RSP_3, 8
  %ESP_3 = trunc i64 %RSP_4 to i32
  %SP_3 = trunc i64 %RSP_4 to i16
  %SPL_3 = trunc i64 %RSP_4 to i8
  %RIP_8 = add i64 %RIP_7, 7
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RIP_9 = add i64 %RIP_8, 7
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %RIP_10 = add i64 %RIP_9, 7
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %RIP_11 = add i64 %RIP_10, 6
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %22 = add i64 %RIP_11, 2100198
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195338, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 6, i8* %CH
  store i8 16, i8* %CL
  store i16 1552, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1232, i16* %DI
  store i8 -48, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195856, i32* %ECX
  store i32 4195536, i32* %EDI
  store i32 %EDX_0, i32* %EDX
  %ZF_0 = icmp eq i64 %RSP_2, 0
  %SF_0 = icmp slt i64 %RSP_2, 0
  %29 = trunc i64 %RSP_2 to i8
  %30 = call i8 @llvm.ctpop.i8(i8 %29)
  %31 = trunc i8 %30 to i1
  %PF_0 = icmp eq i1 %31, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %32 = zext i1 false to i32
  %33 = shl i32 %32, 0
  %34 = or i32 %33, %CtlSysEFLAGS_0
  %35 = zext i1 %PF_0 to i32
  %36 = shl i32 %35, 2
  %37 = or i32 %36, %34
  %38 = zext i1 false to i32
  %39 = shl i32 %38, 4
  %40 = or i32 %39, %37
  %41 = zext i1 %ZF_0 to i32
  %42 = shl i32 %41, 6
  %43 = or i32 %42, %40
  %44 = zext i1 %SF_0 to i32
  %45 = shl i32 %44, 7
  %46 = or i32 %45, %43
  %47 = zext i1 false to i32
  %48 = shl i32 %47, 11
  %EFLAGS_1 = or i32 %48, %46
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_10, i32* %EIP
  store i32 %ESI_0, i32* %ESI
  store i32 %ESP_4, i32* %ESP
  store i16 %IP_10, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 4195856, i64* %RCX
  store i64 4195536, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195968, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 -128, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195968, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1664, i16* %R8W
  store i16 %R9W_0, i16* %R9W
  %49 = load i32, i32* %CtlSysEFLAGS
  store i32 %49, i32* %CtlSysEFLAGS_ptr
  %50 = load i32, i32* %EFLAGS
  store i32 %50, i32* %EFLAGS_ptr
  %51 = load i64, i64* %RAX
  store i64 %51, i64* %RAX_ptr
  %52 = load i64, i64* %RBP
  store i64 %52, i64* %RBP_ptr
  %53 = load i64, i64* %RCX
  store i64 %53, i64* %RCX_ptr
  %54 = load i64, i64* %RDI
  store i64 %54, i64* %RDI_ptr
  %55 = load i64, i64* %RDX
  store i64 %55, i64* %RDX_ptr
  %56 = load i64, i64* %RIP
  store i64 %56, i64* %RIP_ptr
  %57 = load i64, i64* %RSI
  store i64 %57, i64* %RSI_ptr
  %58 = load i64, i64* %RSP
  store i64 %58, i64* %RSP_ptr
  %59 = load i64, i64* %R8
  store i64 %59, i64* %R8_ptr
  %60 = load i64, i64* %R9
  store i64 %60, i64* %R9_ptr
  call void %28(%regset* %0)
  %61 = load i32, i32* %CtlSysEFLAGS_ptr
  store i32 %61, i32* %CtlSysEFLAGS
  %62 = load i32, i32* %EFLAGS_ptr
  store i32 %62, i32* %EFLAGS
  %63 = load i64, i64* %RAX_ptr
  store i64 %63, i64* %RAX
  %64 = load i64, i64* %RBP_ptr
  store i64 %64, i64* %RBP
  %65 = load i64, i64* %RCX_ptr
  store i64 %65, i64* %RCX
  %66 = load i64, i64* %RDI_ptr
  store i64 %66, i64* %RDI
  %67 = load i64, i64* %RDX_ptr
  store i64 %67, i64* %RDX
  %68 = load i64, i64* %RIP_ptr
  store i64 %68, i64* %RIP
  %69 = load i64, i64* %RSI_ptr
  store i64 %69, i64* %RSI
  %70 = load i64, i64* %RSP_ptr
  store i64 %70, i64* %RSP
  %71 = load i64, i64* %R8_ptr
  store i64 %71, i64* %R8
  %72 = load i64, i64* %R9_ptr
  store i64 %72, i64* %R9
  %RIP_12 = load i64, i64* %RIP
  %RIP_13 = add i64 %RIP_12, 1
  %EIP_11 = trunc i64 %RIP_13 to i32
  %IP_11 = trunc i64 %RIP_13 to i16
  call void @llvm.trap()
  %RIP_14 = add i64 %RIP_13, 5
  %EIP_12 = trunc i64 %RIP_14 to i32
  %IP_12 = trunc i64 %RIP_14 to i16
  %RIP_15 = add i64 %RIP_14, 1
  %EIP_13 = trunc i64 %RIP_15 to i32
  %IP_13 = trunc i64 %RIP_15 to i16
  %RIP_16 = add i64 %RIP_15, 1
  %EIP_14 = trunc i64 %RIP_16 to i32
  %IP_14 = trunc i64 %RIP_16 to i16
  %RSP_6 = load i64, i64* %RSP
  %RSP_7 = add i64 %RSP_6, 8
  %73 = inttoptr i64 %RSP_6 to i64*
  %RIP_17 = load i64, i64* %73
  %ESP_5 = trunc i64 %RSP_7 to i32
  %SP_5 = trunc i64 %RSP_7 to i16
  %SPL_5 = trunc i64 %RSP_7 to i8
  %EIP_15 = trunc i64 %RIP_17 to i32
  %IP_15 = trunc i64 %RIP_17 to i16
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EIP_15, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_15, i16* %IP
  store i64 %RIP_17, i64* %RIP
  store i64 %RSP_7, i64* %RSP
  store i16 %SP_5, i16* %SP
  store i8 %SPL_5, i8* %SPL
  br label %exit_fn_4003E0
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
