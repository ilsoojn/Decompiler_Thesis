; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
  %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
  %ZMM0 = alloca <16 x float>
  store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
  %2 = bitcast <16 x float> %ZMM0_init to i512
  %3 = trunc i512 %2 to i128
  %XMM0_init = bitcast i128 %3 to <4 x float>
  %XMM0 = alloca <4 x float>
  store <4 x float> %XMM0_init, <4 x float>* %XMM0
  %4 = bitcast <16 x float> %ZMM0_init to i512
  %5 = trunc i512 %4 to i256
  %YMM0_init = bitcast i256 %5 to <8 x float>
  %YMM0 = alloca <8 x float>
  store <8 x float> %YMM0_init, <8 x float>* %YMM0
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %11 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %11 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400480
  %12 = load i32, i32* %CtlSysEFLAGS
  store i32 %12, i32* %CtlSysEFLAGS_ptr
  %13 = load i32, i32* %EFLAGS
  store i32 %13, i32* %EFLAGS_ptr
  %14 = load i64, i64* %RAX
  store i64 %14, i64* %RAX_ptr
  %15 = load i64, i64* %RBP
  store i64 %15, i64* %RBP_ptr
  %16 = load i64, i64* %RCX
  store i64 %16, i64* %RCX_ptr
  %17 = load i64, i64* %RDX
  store i64 %17, i64* %RDX_ptr
  %18 = load i64, i64* %RIP
  store i64 %18, i64* %RIP_ptr
  %19 = load i64, i64* %RSI
  store i64 %19, i64* %RSI_ptr
  %20 = load i64, i64* %RSP
  store i64 %20, i64* %RSP_ptr
  %21 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %21, <16 x float>* %ZMM0_ptr
  %22 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %22, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %23 = sub i64 %RSP_0, 8
  %24 = inttoptr i64 %23 to i64*
  store i64 %RBP_0, i64* %24, align 1
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
  %RIP_3 = add i64 %RIP_2, 2
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %EAX_1 = xor i32 %EAX_0, %EAX_0
  %RAX_1 = zext i32 %EAX_1 to i64
  %AX_0 = trunc i32 %EAX_1 to i16
  %AL_0 = trunc i32 %EAX_1 to i8
  %25 = lshr i32 %EAX_1, 8
  %AH_0 = trunc i32 %25 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %26 = add i64 %RIP_4, 234
  %27 = inttoptr i64 %26 to double*
  %28 = load double, double* %27, align 1
  %29 = bitcast double %28 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %30 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %30 to i128
  %31 = zext i64 %29 to i128
  %32 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %31, %32
  %33 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %33 to i256
  %34 = zext i128 %XMM0_1 to i256
  %35 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %34, %35
  %36 = bitcast <16 x float> %ZMM0_0 to i512
  %37 = zext i128 %XMM0_1 to i512
  %38 = and i512 %36, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %37, %38
  %RIP_5 = add i64 %RIP_4, 8
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %39 = add i64 %RIP_5, 234
  %40 = inttoptr i64 %39 to float*
  %41 = load float, float* %40, align 1
  %42 = bitcast float %41 to i32
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %43 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %43 to i128
  %44 = zext i32 %42 to i128
  %45 = and i128 %XMM1_0, -4294967296
  %XMM1_1 = or i128 %44, %45
  %46 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %46 to i256
  %47 = zext i128 %XMM1_1 to i256
  %48 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %47, %48
  %49 = bitcast <16 x float> %ZMM1_0 to i512
  %50 = zext i128 %XMM1_1 to i512
  %51 = and i512 %49, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %50, %51
  %RIP_6 = add i64 %RIP_5, 10
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RDX_0 = load i64, i64* %RDX
  %RIP_8 = add i64 %RIP_7, 2
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_9 = add i64 %RIP_8, 6
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %52 = add i64 %RSP_1, -2
  %53 = inttoptr i64 %52 to i16*
  store i16 -32100, i16* %53, align 1
  %RIP_10 = add i64 %RIP_9, 6
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %54 = add i64 %RSP_1, -4
  %55 = inttoptr i64 %54 to i16*
  store i16 -104, i16* %55, align 1
  %RIP_11 = add i64 %RIP_10, 7
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %56 = add i64 %RSP_1, -8
  %57 = inttoptr i64 %56 to i32*
  store i32 -4967296, i32* %57, align 1
  %RIP_12 = add i64 %RIP_11, 7
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %58 = add i64 %RSP_1, -12
  %59 = inttoptr i64 %58 to i32*
  store i32 -2123123123, i32* %59, align 1
  %RIP_13 = add i64 %RIP_12, 8
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %60 = add i64 %RSP_1, -24
  %61 = inttoptr i64 %60 to i64*
  store i64 -2111111111, i64* %61, align 1
  %RIP_14 = add i64 %RIP_13, 4
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %62 = add i64 %RSP_1, -32
  %63 = inttoptr i64 %62 to i64*
  store i64 4200000000, i64* %63, align 1
  %RIP_15 = add i64 %RIP_14, 8
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %64 = add i64 %RSP_1, -40
  %65 = inttoptr i64 %64 to i64*
  store i64 -123123123, i64* %65, align 1
  %RIP_16 = add i64 %RIP_15, 4
  %EIP_15 = trunc i64 %RIP_16 to i32
  %IP_15 = trunc i64 %RIP_16 to i16
  %66 = add i64 %RSP_1, -48
  %67 = inttoptr i64 %66 to i64*
  store i64 123123123123, i64* %67, align 1
  %RIP_17 = add i64 %RIP_16, 4
  %EIP_16 = trunc i64 %RIP_17 to i32
  %IP_16 = trunc i64 %RIP_17 to i16
  %68 = add i64 %RSP_1, -49
  %69 = inttoptr i64 %68 to i8*
  store i8 32, i8* %69, align 1
  %RIP_18 = add i64 %RIP_17, 4
  %EIP_17 = trunc i64 %RIP_18 to i32
  %IP_17 = trunc i64 %RIP_18 to i16
  %70 = add i64 %RSP_1, -50
  %71 = inttoptr i64 %70 to i8*
  store i8 33, i8* %71, align 1
  %RIP_19 = add i64 %RIP_18, 5
  %EIP_18 = trunc i64 %RIP_19 to i32
  %IP_18 = trunc i64 %RIP_19 to i16
  %72 = trunc i128 %XMM1_1 to i32
  %73 = bitcast i32 %72 to float
  %74 = add i64 %RSP_1, -56
  %75 = inttoptr i64 %74 to float*
  store float %73, float* %75, align 1
  %RIP_20 = add i64 %RIP_19, 5
  %EIP_19 = trunc i64 %RIP_20 to i32
  %IP_19 = trunc i64 %RIP_20 to i16
  %76 = trunc i128 %XMM0_1 to i64
  %77 = bitcast i64 %76 to double
  %78 = add i64 %RSP_1, -64
  %79 = inttoptr i64 %78 to double*
  store double %77, double* %79, align 1
  %RIP_21 = add i64 %RIP_20, 1
  %EIP_20 = trunc i64 %RIP_21 to i32
  %IP_20 = trunc i64 %RIP_21 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %80 = sub i64 %RSP_2, 8
  %81 = inttoptr i64 %80 to i64*
  %RBP_1 = load i64, i64* %81, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_22 = add i64 %RIP_21, 1
  %EIP_21 = trunc i64 %RIP_22 to i32
  %IP_21 = trunc i64 %RIP_22 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %82 = inttoptr i64 %RSP_2 to i64*
  %RIP_23 = load i64, i64* %82
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_22 = trunc i64 %RIP_23 to i32
  %IP_22 = trunc i64 %RIP_23 to i16
  %ZF_0 = icmp eq i32 %EAX_1, 0
  %SF_0 = icmp slt i32 %EAX_1, 0
  %83 = trunc i32 %EAX_1 to i8
  %84 = call i8 @llvm.ctpop.i8(i8 %83)
  %85 = trunc i8 %84 to i1
  %PF_0 = icmp eq i1 %85, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %86 = zext i1 false to i32
  %87 = shl i32 %86, 0
  %88 = or i32 %87, %CtlSysEFLAGS_0
  %89 = zext i1 %PF_0 to i32
  %90 = shl i32 %89, 2
  %91 = or i32 %90, %88
  %92 = zext i1 false to i32
  %93 = shl i32 %92, 4
  %94 = or i32 %93, %91
  %95 = zext i1 %ZF_0 to i32
  %96 = shl i32 %95, 6
  %97 = or i32 %96, %94
  %98 = zext i1 %SF_0 to i32
  %99 = shl i32 %98, 7
  %100 = or i32 %99, %97
  %101 = zext i1 false to i32
  %102 = shl i32 %101, 11
  %EFLAGS_1 = or i32 %102, %100
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 -61, i8* %CH
  store i8 -77, i8* %CL
  store i16 -15437, i16* %CX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i8 -22, i8* %DH
  store i8 0, i8* %DL
  store i16 -5632, i16* %DX
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 -1430928461, i32* %ECX
  store i32 -94967296, i32* %EDX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_22, i32* %EIP
  store i32 -94967296, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_22, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 123123123123, i64* %RCX
  store i64 4200000000, i64* %RDX
  store i64 %RIP_23, i64* %RIP
  store i64 4200000000, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  store i16 -5632, i16* %SI
  store i8 0, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  %103 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %103, <4 x float>* %XMM0
  %104 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %104, <4 x float>* %XMM1
  %105 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %105, <8 x float>* %YMM0
  %106 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %106, <8 x float>* %YMM1
  %107 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %107, <16 x float>* %ZMM0
  %108 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %108, <16 x float>* %ZMM1
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
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
  br label %bb_4003D0

exit_fn_4003D0:                                   ; preds = %bb_4003EB, %bb_4003F8
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

bb_4003D0:                                        ; preds = %entry_fn_4003D0
  %RIP_1 = add i64 4195280, 1
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
  %CC_A_0 = icmp ugt i64 6295592, 6295592
  %CC_AE_0 = icmp uge i64 6295592, 6295592
  %CC_B_0 = icmp ult i64 6295592, 6295592
  %CC_BE_0 = icmp ule i64 6295592, 6295592
  %CC_L_0 = icmp slt i64 6295592, 6295592
  %CC_LE_0 = icmp sle i64 6295592, 6295592
  %CC_G_0 = icmp sgt i64 6295592, 6295592
  %CC_GE_0 = icmp sge i64 6295592, 6295592
  %CC_E_0 = icmp eq i64 6295592, 6295592
  %CC_NE_0 = icmp ne i64 6295592, 6295592
  %11 = sub i64 6295592, 6295592
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295592, i64 6295592)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295592, i64 6295592)
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
  store i8 40, i8* %AL
  store i16 4136, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295592, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195320, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1016, i16* %IP
  store i64 6295592, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195320, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003F8, label %bb_4003E1

bb_4003E1:                                        ; preds = %bb_4003D0
  %RIP_8 = add i64 4195297, 5
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
  store i32 4195320, i32* %EIP
  store i16 1016, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195320, i64* %RIP
  br i1 %CC_E_09, label %bb_4003F8, label %bb_4003EB

bb_4003EB:                                        ; preds = %bb_4003E1
  %RIP_17 = add i64 4195307, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_4003D0

bb_4003F8:                                        ; preds = %bb_4003E1, %bb_4003D0
  %RIP_13 = add i64 4195320, 1
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
  br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define void @fn_400400(%regset* noalias nocapture) {
entry_fn_400400:
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
  br label %bb_400400

exit_fn_400400:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400400:                                        ; preds = %entry_fn_400400
  %RIP_1 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_13 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_22 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400400

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_18 = add i64 4195384, 1
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
  br label %exit_fn_400400
}

define void @fn_400440(%regset* noalias nocapture) {
entry_fn_400440:
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
  br label %bb_400440

exit_fn_400440:                                   ; preds = %bb_400460, %bb_400449
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

bb_400440:                                        ; preds = %entry_fn_400440
  %RIP_1 = add i64 4195392, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100193
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
  store i32 4195424, i32* %EIP
  store i16 1120, i16* %IP
  store i64 4195424, i64* %RIP
  br i1 %CC_NE_0, label %bb_400460, label %bb_400449

bb_400449:                                        ; preds = %bb_400440
  %RIP_5 = add i64 4195401, 1
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
  store i64 4195410, i64* %33
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
  call void @fn_4003D0(%regset* %0)
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
  %44 = add i64 %RIP_9, 2100175
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
  br label %exit_fn_400440

bb_400460:                                        ; preds = %bb_400440
  %RIP_14 = add i64 4195424, 1
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
  br label %exit_fn_400440
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

define void @fn_400470(%regset* noalias nocapture) {
entry_fn_400470:
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
  br label %bb_400470

exit_fn_400470:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400470:                                        ; preds = %entry_fn_400470
  %RIP_1 = add i64 4195440, 1
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
  store i32 4195328, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1024, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195328, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400400

bb_400400:                                        ; preds = %bb_400470
  %RIP_7 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_19 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_28 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400470

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_24 = add i64 4195384, 1
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
  br label %exit_fn_400470
}

define void @fn_400560(%regset* noalias nocapture) {
entry_fn_400560:
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
  br label %bb_400560

exit_fn_400560:                                   ; preds = %bb_400560
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400560:                                        ; preds = %entry_fn_400560
  %RIP_1 = add i64 4195680, 1
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
  br label %exit_fn_400560
}

define void @fn_400564(%regset* noalias nocapture) {
entry_fn_400564:
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
  br label %bb_400564

exit_fn_400564:                                   ; preds = %bb_400564
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400564:                                        ; preds = %entry_fn_400564
  %RIP_1 = add i64 4195684, 4
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
  br label %exit_fn_400564
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

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
  br label %bb_4004F0

exit_fn_4004F0:                                   ; preds = %bb_400546
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

bb_4004F0:                                        ; preds = %entry_fn_4004F0
  %RIP_1 = add i64 4195568, 2
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
  %R12_1 = add i64 %RIP_6, 2099534
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
  %RBP_1 = add i64 %RIP_8, 2099534
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
  store i64 4195617, i64* %29
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
  call void @fn_400370(%regset* %0)
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
  store i32 4195654, i32* %EIP
  store i16 1350, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195654, i64* %RIP
  br i1 %CC_E_0, label %bb_400546, label %bb_400526

bb_400526:                                        ; preds = %bb_4004F0
  %RIP_21 = add i64 4195622, 2
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
  br label %bb_400530

bb_400530:                                        ; preds = %bb_400530, %bb_400526
  %RIP_34 = add i64 4195632, 3
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
  store i64 4195645, i64* %126
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
  store i32 4195632, i32* %EIP
  store i16 1328, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195632, i64* %RIP
  br i1 %CC_NE_016, label %bb_400530, label %bb_400546

bb_400546:                                        ; preds = %bb_400530, %bb_4004F0
  %RIP_24 = add i64 4195654, 4
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
  br label %exit_fn_4004F0
}

define void @fn_400370(%regset* noalias nocapture) {
entry_fn_400370:
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
  br label %bb_400370

exit_fn_400370:                                   ; preds = %bb_400382
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

bb_400370:                                        ; preds = %entry_fn_400370
  %RIP_1 = add i64 4195184, 4
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
  %7 = add i64 %RIP_2, 2100349
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
  store i32 4195202, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 898, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195202, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400382, label %bb_400380

bb_400380:                                        ; preds = %bb_400370
  %RIP_7 = add i64 4195200, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195202, i64* %34
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
  br label %bb_400382

bb_400382:                                        ; preds = %bb_400380, %bb_400370
  %RIP_9 = add i64 4195202, 4
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
  br label %exit_fn_400370
}

define void @fn_4003C0(%regset* noalias nocapture) {
entry_fn_4003C0:
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
  br label %bb_4003C0

exit_fn_4003C0:                                   ; preds = %bb_4003C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_4003C0:                                        ; preds = %entry_fn_4003C0
  %RIP_1 = add i64 4195264, 1
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
  br label %exit_fn_4003C0
}

define void @fn_400390(%regset* noalias nocapture) {
entry_fn_400390:
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
  br label %bb_400390

exit_fn_400390:                                   ; preds = %bb_400390
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

bb_400390:                                        ; preds = %entry_fn_400390
  %RIP_1 = add i64 4195216, 2
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
  %22 = add i64 %RIP_11, 2100278
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195258, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 4, i8* %CH
  store i8 -16, i8* %CL
  store i16 1264, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1152, i16* %DI
  store i8 -128, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195568, i32* %ECX
  store i32 4195456, i32* %EDI
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
  store i64 4195568, i64* %RCX
  store i64 4195456, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195680, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 96, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195680, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1376, i16* %R8W
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
  br label %exit_fn_400390
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
  %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
  %ZMM0 = alloca <16 x float>
  store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
  %2 = bitcast <16 x float> %ZMM0_init to i512
  %3 = trunc i512 %2 to i128
  %XMM0_init = bitcast i128 %3 to <4 x float>
  %XMM0 = alloca <4 x float>
  store <4 x float> %XMM0_init, <4 x float>* %XMM0
  %4 = bitcast <16 x float> %ZMM0_init to i512
  %5 = trunc i512 %4 to i256
  %YMM0_init = bitcast i256 %5 to <8 x float>
  %YMM0 = alloca <8 x float>
  store <8 x float> %YMM0_init, <8 x float>* %YMM0
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %11 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %11 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400480
  %12 = load i32, i32* %CtlSysEFLAGS
  store i32 %12, i32* %CtlSysEFLAGS_ptr
  %13 = load i32, i32* %EFLAGS
  store i32 %13, i32* %EFLAGS_ptr
  %14 = load i64, i64* %RAX
  store i64 %14, i64* %RAX_ptr
  %15 = load i64, i64* %RBP
  store i64 %15, i64* %RBP_ptr
  %16 = load i64, i64* %RCX
  store i64 %16, i64* %RCX_ptr
  %17 = load i64, i64* %RDX
  store i64 %17, i64* %RDX_ptr
  %18 = load i64, i64* %RIP
  store i64 %18, i64* %RIP_ptr
  %19 = load i64, i64* %RSI
  store i64 %19, i64* %RSI_ptr
  %20 = load i64, i64* %RSP
  store i64 %20, i64* %RSP_ptr
  %21 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %21, <16 x float>* %ZMM0_ptr
  %22 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %22, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %23 = sub i64 %RSP_0, 8
  %24 = inttoptr i64 %23 to i64*
  store i64 %RBP_0, i64* %24, align 1
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
  %RIP_3 = add i64 %RIP_2, 2
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %EAX_1 = xor i32 %EAX_0, %EAX_0
  %RAX_1 = zext i32 %EAX_1 to i64
  %AX_0 = trunc i32 %EAX_1 to i16
  %AL_0 = trunc i32 %EAX_1 to i8
  %25 = lshr i32 %EAX_1, 8
  %AH_0 = trunc i32 %25 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %26 = add i64 %RIP_4, 250
  %27 = inttoptr i64 %26 to double*
  %28 = load double, double* %27, align 1
  %29 = bitcast double %28 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %30 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %30 to i128
  %31 = zext i64 %29 to i128
  %32 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %31, %32
  %33 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %33 to i256
  %34 = zext i128 %XMM0_1 to i256
  %35 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %34, %35
  %36 = bitcast <16 x float> %ZMM0_0 to i512
  %37 = zext i128 %XMM0_1 to i512
  %38 = and i512 %36, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %37, %38
  %RIP_5 = add i64 %RIP_4, 8
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %39 = add i64 %RIP_5, 250
  %40 = inttoptr i64 %39 to float*
  %41 = load float, float* %40, align 1
  %42 = bitcast float %41 to i32
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %43 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %43 to i128
  %44 = zext i32 %42 to i128
  %45 = and i128 %XMM1_0, -4294967296
  %XMM1_1 = or i128 %44, %45
  %46 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %46 to i256
  %47 = zext i128 %XMM1_1 to i256
  %48 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %47, %48
  %49 = bitcast <16 x float> %ZMM1_0 to i512
  %50 = zext i128 %XMM1_1 to i512
  %51 = and i512 %49, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %50, %51
  %RIP_6 = add i64 %RIP_5, 10
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RDX_0 = load i64, i64* %RDX
  %RIP_8 = add i64 %RIP_7, 2
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_9 = add i64 %RIP_8, 7
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %52 = add i64 %RSP_1, -4
  %53 = inttoptr i64 %52 to i32*
  store i32 0, i32* %53, align 1
  %RIP_10 = add i64 %RIP_9, 6
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %54 = add i64 %RSP_1, -6
  %55 = inttoptr i64 %54 to i16*
  store i16 -32100, i16* %55, align 1
  %RIP_11 = add i64 %RIP_10, 6
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %56 = add i64 %RSP_1, -8
  %57 = inttoptr i64 %56 to i16*
  store i16 -104, i16* %57, align 1
  %RIP_12 = add i64 %RIP_11, 7
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %58 = add i64 %RSP_1, -12
  %59 = inttoptr i64 %58 to i32*
  store i32 -4967296, i32* %59, align 1
  %RIP_13 = add i64 %RIP_12, 7
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %60 = add i64 %RSP_1, -16
  %61 = inttoptr i64 %60 to i32*
  store i32 -2123123123, i32* %61, align 1
  %RIP_14 = add i64 %RIP_13, 8
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %62 = add i64 %RSP_1, -24
  %63 = inttoptr i64 %62 to i64*
  store i64 -2111111111, i64* %63, align 1
  %RIP_15 = add i64 %RIP_14, 4
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %64 = add i64 %RSP_1, -32
  %65 = inttoptr i64 %64 to i64*
  store i64 4200000000, i64* %65, align 1
  %RIP_16 = add i64 %RIP_15, 8
  %EIP_15 = trunc i64 %RIP_16 to i32
  %IP_15 = trunc i64 %RIP_16 to i16
  %66 = add i64 %RSP_1, -40
  %67 = inttoptr i64 %66 to i64*
  store i64 -123123123, i64* %67, align 1
  %RIP_17 = add i64 %RIP_16, 4
  %EIP_16 = trunc i64 %RIP_17 to i32
  %IP_16 = trunc i64 %RIP_17 to i16
  %68 = add i64 %RSP_1, -48
  %69 = inttoptr i64 %68 to i64*
  store i64 123123123123, i64* %69, align 1
  %RIP_18 = add i64 %RIP_17, 4
  %EIP_17 = trunc i64 %RIP_18 to i32
  %IP_17 = trunc i64 %RIP_18 to i16
  %70 = add i64 %RSP_1, -49
  %71 = inttoptr i64 %70 to i8*
  store i8 32, i8* %71, align 1
  %RIP_19 = add i64 %RIP_18, 4
  %EIP_18 = trunc i64 %RIP_19 to i32
  %IP_18 = trunc i64 %RIP_19 to i16
  %72 = add i64 %RSP_1, -50
  %73 = inttoptr i64 %72 to i8*
  store i8 33, i8* %73, align 1
  %RIP_20 = add i64 %RIP_19, 5
  %EIP_19 = trunc i64 %RIP_20 to i32
  %IP_19 = trunc i64 %RIP_20 to i16
  %74 = trunc i128 %XMM1_1 to i32
  %75 = bitcast i32 %74 to float
  %76 = add i64 %RSP_1, -56
  %77 = inttoptr i64 %76 to float*
  store float %75, float* %77, align 1
  %RIP_21 = add i64 %RIP_20, 5
  %EIP_20 = trunc i64 %RIP_21 to i32
  %IP_20 = trunc i64 %RIP_21 to i16
  %78 = trunc i128 %XMM0_1 to i64
  %79 = bitcast i64 %78 to double
  %80 = add i64 %RSP_1, -64
  %81 = inttoptr i64 %80 to double*
  store double %79, double* %81, align 1
  %RIP_22 = add i64 %RIP_21, 1
  %EIP_21 = trunc i64 %RIP_22 to i32
  %IP_21 = trunc i64 %RIP_22 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %82 = sub i64 %RSP_2, 8
  %83 = inttoptr i64 %82 to i64*
  %RBP_1 = load i64, i64* %83, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_23 = add i64 %RIP_22, 1
  %EIP_22 = trunc i64 %RIP_23 to i32
  %IP_22 = trunc i64 %RIP_23 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %84 = inttoptr i64 %RSP_2 to i64*
  %RIP_24 = load i64, i64* %84
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_23 = trunc i64 %RIP_24 to i32
  %IP_23 = trunc i64 %RIP_24 to i16
  %ZF_0 = icmp eq i32 %EAX_1, 0
  %SF_0 = icmp slt i32 %EAX_1, 0
  %85 = trunc i32 %EAX_1 to i8
  %86 = call i8 @llvm.ctpop.i8(i8 %85)
  %87 = trunc i8 %86 to i1
  %PF_0 = icmp eq i1 %87, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %88 = zext i1 false to i32
  %89 = shl i32 %88, 0
  %90 = or i32 %89, %CtlSysEFLAGS_0
  %91 = zext i1 %PF_0 to i32
  %92 = shl i32 %91, 2
  %93 = or i32 %92, %90
  %94 = zext i1 false to i32
  %95 = shl i32 %94, 4
  %96 = or i32 %95, %93
  %97 = zext i1 %ZF_0 to i32
  %98 = shl i32 %97, 6
  %99 = or i32 %98, %96
  %100 = zext i1 %SF_0 to i32
  %101 = shl i32 %100, 7
  %102 = or i32 %101, %99
  %103 = zext i1 false to i32
  %104 = shl i32 %103, 11
  %EFLAGS_1 = or i32 %104, %102
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 -61, i8* %CH
  store i8 -77, i8* %CL
  store i16 -15437, i16* %CX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i8 -22, i8* %DH
  store i8 0, i8* %DL
  store i16 -5632, i16* %DX
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 -1430928461, i32* %ECX
  store i32 -94967296, i32* %EDX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_23, i32* %EIP
  store i32 -94967296, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_23, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 123123123123, i64* %RCX
  store i64 4200000000, i64* %RDX
  store i64 %RIP_24, i64* %RIP
  store i64 4200000000, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  store i16 -5632, i16* %SI
  store i8 0, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  %105 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %105, <4 x float>* %XMM0
  %106 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %106, <4 x float>* %XMM1
  %107 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %107, <8 x float>* %YMM0
  %108 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %108, <8 x float>* %YMM1
  %109 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %109, <16 x float>* %ZMM0
  %110 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %110, <16 x float>* %ZMM1
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
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
  br label %bb_4003D0

exit_fn_4003D0:                                   ; preds = %bb_4003EB, %bb_4003F8
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

bb_4003D0:                                        ; preds = %entry_fn_4003D0
  %RIP_1 = add i64 4195280, 1
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
  %CC_A_0 = icmp ugt i64 6295592, 6295592
  %CC_AE_0 = icmp uge i64 6295592, 6295592
  %CC_B_0 = icmp ult i64 6295592, 6295592
  %CC_BE_0 = icmp ule i64 6295592, 6295592
  %CC_L_0 = icmp slt i64 6295592, 6295592
  %CC_LE_0 = icmp sle i64 6295592, 6295592
  %CC_G_0 = icmp sgt i64 6295592, 6295592
  %CC_GE_0 = icmp sge i64 6295592, 6295592
  %CC_E_0 = icmp eq i64 6295592, 6295592
  %CC_NE_0 = icmp ne i64 6295592, 6295592
  %11 = sub i64 6295592, 6295592
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295592, i64 6295592)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295592, i64 6295592)
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
  store i8 40, i8* %AL
  store i16 4136, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295592, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195320, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1016, i16* %IP
  store i64 6295592, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195320, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003F8, label %bb_4003E1

bb_4003E1:                                        ; preds = %bb_4003D0
  %RIP_8 = add i64 4195297, 5
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
  store i32 4195320, i32* %EIP
  store i16 1016, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195320, i64* %RIP
  br i1 %CC_E_09, label %bb_4003F8, label %bb_4003EB

bb_4003EB:                                        ; preds = %bb_4003E1
  %RIP_17 = add i64 4195307, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_4003D0

bb_4003F8:                                        ; preds = %bb_4003E1, %bb_4003D0
  %RIP_13 = add i64 4195320, 1
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
  br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define void @fn_400400(%regset* noalias nocapture) {
entry_fn_400400:
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
  br label %bb_400400

exit_fn_400400:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400400:                                        ; preds = %entry_fn_400400
  %RIP_1 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_13 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_22 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400400

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_18 = add i64 4195384, 1
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
  br label %exit_fn_400400
}

define void @fn_400440(%regset* noalias nocapture) {
entry_fn_400440:
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
  br label %bb_400440

exit_fn_400440:                                   ; preds = %bb_400460, %bb_400449
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

bb_400440:                                        ; preds = %entry_fn_400440
  %RIP_1 = add i64 4195392, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100193
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
  store i32 4195424, i32* %EIP
  store i16 1120, i16* %IP
  store i64 4195424, i64* %RIP
  br i1 %CC_NE_0, label %bb_400460, label %bb_400449

bb_400449:                                        ; preds = %bb_400440
  %RIP_5 = add i64 4195401, 1
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
  store i64 4195410, i64* %33
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
  call void @fn_4003D0(%regset* %0)
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
  %44 = add i64 %RIP_9, 2100175
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
  br label %exit_fn_400440

bb_400460:                                        ; preds = %bb_400440
  %RIP_14 = add i64 4195424, 1
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
  br label %exit_fn_400440
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

define void @fn_400470(%regset* noalias nocapture) {
entry_fn_400470:
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
  br label %bb_400470

exit_fn_400470:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400470:                                        ; preds = %entry_fn_400470
  %RIP_1 = add i64 4195440, 1
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
  store i32 4195328, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1024, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195328, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400400

bb_400400:                                        ; preds = %bb_400470
  %RIP_7 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_19 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_28 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400470

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_24 = add i64 4195384, 1
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
  br label %exit_fn_400470
}

define void @fn_400570(%regset* noalias nocapture) {
entry_fn_400570:
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
  br label %bb_400570

exit_fn_400570:                                   ; preds = %bb_400570
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400570:                                        ; preds = %entry_fn_400570
  %RIP_1 = add i64 4195696, 1
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
  br label %exit_fn_400570
}

define void @fn_400574(%regset* noalias nocapture) {
entry_fn_400574:
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
  br label %bb_400574

exit_fn_400574:                                   ; preds = %bb_400574
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400574:                                        ; preds = %entry_fn_400574
  %RIP_1 = add i64 4195700, 4
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
  br label %exit_fn_400574
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

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
  br label %bb_400500

exit_fn_400500:                                   ; preds = %bb_400556
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

bb_400500:                                        ; preds = %entry_fn_400500
  %RIP_1 = add i64 4195584, 2
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
  %R12_1 = add i64 %RIP_6, 2099518
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
  %RBP_1 = add i64 %RIP_8, 2099518
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
  store i64 4195633, i64* %29
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
  call void @fn_400370(%regset* %0)
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
  store i32 4195670, i32* %EIP
  store i16 1366, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195670, i64* %RIP
  br i1 %CC_E_0, label %bb_400556, label %bb_400536

bb_400536:                                        ; preds = %bb_400500
  %RIP_21 = add i64 4195638, 2
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
  br label %bb_400540

bb_400540:                                        ; preds = %bb_400540, %bb_400536
  %RIP_34 = add i64 4195648, 3
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
  store i64 4195661, i64* %126
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
  store i32 4195648, i32* %EIP
  store i16 1344, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195648, i64* %RIP
  br i1 %CC_NE_016, label %bb_400540, label %bb_400556

bb_400556:                                        ; preds = %bb_400540, %bb_400500
  %RIP_24 = add i64 4195670, 4
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
  br label %exit_fn_400500
}

define void @fn_400370(%regset* noalias nocapture) {
entry_fn_400370:
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
  br label %bb_400370

exit_fn_400370:                                   ; preds = %bb_400382
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

bb_400370:                                        ; preds = %entry_fn_400370
  %RIP_1 = add i64 4195184, 4
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
  %7 = add i64 %RIP_2, 2100349
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
  store i32 4195202, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 898, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195202, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400382, label %bb_400380

bb_400380:                                        ; preds = %bb_400370
  %RIP_7 = add i64 4195200, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195202, i64* %34
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
  br label %bb_400382

bb_400382:                                        ; preds = %bb_400380, %bb_400370
  %RIP_9 = add i64 4195202, 4
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
  br label %exit_fn_400370
}

define void @fn_4003C0(%regset* noalias nocapture) {
entry_fn_4003C0:
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
  br label %bb_4003C0

exit_fn_4003C0:                                   ; preds = %bb_4003C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_4003C0:                                        ; preds = %entry_fn_4003C0
  %RIP_1 = add i64 4195264, 1
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
  br label %exit_fn_4003C0
}

define void @fn_400390(%regset* noalias nocapture) {
entry_fn_400390:
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
  br label %bb_400390

exit_fn_400390:                                   ; preds = %bb_400390
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

bb_400390:                                        ; preds = %entry_fn_400390
  %RIP_1 = add i64 4195216, 2
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
  %22 = add i64 %RIP_11, 2100278
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195258, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 5, i8* %CH
  store i8 0, i8* %CL
  store i16 1280, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1152, i16* %DI
  store i8 -128, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195584, i32* %ECX
  store i32 4195456, i32* %EDI
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
  store i64 4195584, i64* %RCX
  store i64 4195456, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195696, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 112, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195696, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1392, i16* %R8W
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
  br label %exit_fn_400390
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
  %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
  %ZMM0 = alloca <16 x float>
  store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
  %2 = bitcast <16 x float> %ZMM0_init to i512
  %3 = trunc i512 %2 to i128
  %XMM0_init = bitcast i128 %3 to <4 x float>
  %XMM0 = alloca <4 x float>
  store <4 x float> %XMM0_init, <4 x float>* %XMM0
  %4 = bitcast <16 x float> %ZMM0_init to i512
  %5 = trunc i512 %4 to i256
  %YMM0_init = bitcast i256 %5 to <8 x float>
  %YMM0 = alloca <8 x float>
  store <8 x float> %YMM0_init, <8 x float>* %YMM0
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %11 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %11 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400480
  %12 = load i32, i32* %CtlSysEFLAGS
  store i32 %12, i32* %CtlSysEFLAGS_ptr
  %13 = load i32, i32* %EFLAGS
  store i32 %13, i32* %EFLAGS_ptr
  %14 = load i64, i64* %RAX
  store i64 %14, i64* %RAX_ptr
  %15 = load i64, i64* %RBP
  store i64 %15, i64* %RBP_ptr
  %16 = load i64, i64* %RCX
  store i64 %16, i64* %RCX_ptr
  %17 = load i64, i64* %RDX
  store i64 %17, i64* %RDX_ptr
  %18 = load i64, i64* %RIP
  store i64 %18, i64* %RIP_ptr
  %19 = load i64, i64* %RSI
  store i64 %19, i64* %RSI_ptr
  %20 = load i64, i64* %RSP
  store i64 %20, i64* %RSP_ptr
  %21 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %21, <16 x float>* %ZMM0_ptr
  %22 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %22, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %23 = sub i64 %RSP_0, 8
  %24 = inttoptr i64 %23 to i64*
  store i64 %RBP_0, i64* %24, align 1
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
  %RIP_3 = add i64 %RIP_2, 2
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %EAX_1 = xor i32 %EAX_0, %EAX_0
  %RAX_1 = zext i32 %EAX_1 to i64
  %AX_0 = trunc i32 %EAX_1 to i16
  %AL_0 = trunc i32 %EAX_1 to i8
  %25 = lshr i32 %EAX_1, 8
  %AH_0 = trunc i32 %25 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %26 = add i64 %RIP_4, 250
  %27 = inttoptr i64 %26 to double*
  %28 = load double, double* %27, align 1
  %29 = bitcast double %28 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %30 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %30 to i128
  %31 = zext i64 %29 to i128
  %32 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %31, %32
  %33 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %33 to i256
  %34 = zext i128 %XMM0_1 to i256
  %35 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %34, %35
  %36 = bitcast <16 x float> %ZMM0_0 to i512
  %37 = zext i128 %XMM0_1 to i512
  %38 = and i512 %36, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %37, %38
  %RIP_5 = add i64 %RIP_4, 8
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %39 = add i64 %RIP_5, 250
  %40 = inttoptr i64 %39 to float*
  %41 = load float, float* %40, align 1
  %42 = bitcast float %41 to i32
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %43 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %43 to i128
  %44 = zext i32 %42 to i128
  %45 = and i128 %XMM1_0, -4294967296
  %XMM1_1 = or i128 %44, %45
  %46 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %46 to i256
  %47 = zext i128 %XMM1_1 to i256
  %48 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %47, %48
  %49 = bitcast <16 x float> %ZMM1_0 to i512
  %50 = zext i128 %XMM1_1 to i512
  %51 = and i512 %49, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %50, %51
  %RIP_6 = add i64 %RIP_5, 10
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RDX_0 = load i64, i64* %RDX
  %RIP_8 = add i64 %RIP_7, 2
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_9 = add i64 %RIP_8, 7
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %52 = add i64 %RSP_1, -4
  %53 = inttoptr i64 %52 to i32*
  store i32 0, i32* %53, align 1
  %RIP_10 = add i64 %RIP_9, 6
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %54 = add i64 %RSP_1, -6
  %55 = inttoptr i64 %54 to i16*
  store i16 -32100, i16* %55, align 1
  %RIP_11 = add i64 %RIP_10, 6
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %56 = add i64 %RSP_1, -8
  %57 = inttoptr i64 %56 to i16*
  store i16 -104, i16* %57, align 1
  %RIP_12 = add i64 %RIP_11, 7
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %58 = add i64 %RSP_1, -12
  %59 = inttoptr i64 %58 to i32*
  store i32 -4967296, i32* %59, align 1
  %RIP_13 = add i64 %RIP_12, 7
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %60 = add i64 %RSP_1, -16
  %61 = inttoptr i64 %60 to i32*
  store i32 -2123123123, i32* %61, align 1
  %RIP_14 = add i64 %RIP_13, 8
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %62 = add i64 %RSP_1, -24
  %63 = inttoptr i64 %62 to i64*
  store i64 -2111111111, i64* %63, align 1
  %RIP_15 = add i64 %RIP_14, 4
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %64 = add i64 %RSP_1, -32
  %65 = inttoptr i64 %64 to i64*
  store i64 4200000000, i64* %65, align 1
  %RIP_16 = add i64 %RIP_15, 8
  %EIP_15 = trunc i64 %RIP_16 to i32
  %IP_15 = trunc i64 %RIP_16 to i16
  %66 = add i64 %RSP_1, -40
  %67 = inttoptr i64 %66 to i64*
  store i64 -123123123, i64* %67, align 1
  %RIP_17 = add i64 %RIP_16, 4
  %EIP_16 = trunc i64 %RIP_17 to i32
  %IP_16 = trunc i64 %RIP_17 to i16
  %68 = add i64 %RSP_1, -48
  %69 = inttoptr i64 %68 to i64*
  store i64 123123123123, i64* %69, align 1
  %RIP_18 = add i64 %RIP_17, 4
  %EIP_17 = trunc i64 %RIP_18 to i32
  %IP_17 = trunc i64 %RIP_18 to i16
  %70 = add i64 %RSP_1, -49
  %71 = inttoptr i64 %70 to i8*
  store i8 32, i8* %71, align 1
  %RIP_19 = add i64 %RIP_18, 4
  %EIP_18 = trunc i64 %RIP_19 to i32
  %IP_18 = trunc i64 %RIP_19 to i16
  %72 = add i64 %RSP_1, -50
  %73 = inttoptr i64 %72 to i8*
  store i8 33, i8* %73, align 1
  %RIP_20 = add i64 %RIP_19, 5
  %EIP_19 = trunc i64 %RIP_20 to i32
  %IP_19 = trunc i64 %RIP_20 to i16
  %74 = trunc i128 %XMM1_1 to i32
  %75 = bitcast i32 %74 to float
  %76 = add i64 %RSP_1, -56
  %77 = inttoptr i64 %76 to float*
  store float %75, float* %77, align 1
  %RIP_21 = add i64 %RIP_20, 5
  %EIP_20 = trunc i64 %RIP_21 to i32
  %IP_20 = trunc i64 %RIP_21 to i16
  %78 = trunc i128 %XMM0_1 to i64
  %79 = bitcast i64 %78 to double
  %80 = add i64 %RSP_1, -64
  %81 = inttoptr i64 %80 to double*
  store double %79, double* %81, align 1
  %RIP_22 = add i64 %RIP_21, 1
  %EIP_21 = trunc i64 %RIP_22 to i32
  %IP_21 = trunc i64 %RIP_22 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %82 = sub i64 %RSP_2, 8
  %83 = inttoptr i64 %82 to i64*
  %RBP_1 = load i64, i64* %83, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_23 = add i64 %RIP_22, 1
  %EIP_22 = trunc i64 %RIP_23 to i32
  %IP_22 = trunc i64 %RIP_23 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %84 = inttoptr i64 %RSP_2 to i64*
  %RIP_24 = load i64, i64* %84
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_23 = trunc i64 %RIP_24 to i32
  %IP_23 = trunc i64 %RIP_24 to i16
  %ZF_0 = icmp eq i32 %EAX_1, 0
  %SF_0 = icmp slt i32 %EAX_1, 0
  %85 = trunc i32 %EAX_1 to i8
  %86 = call i8 @llvm.ctpop.i8(i8 %85)
  %87 = trunc i8 %86 to i1
  %PF_0 = icmp eq i1 %87, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %88 = zext i1 false to i32
  %89 = shl i32 %88, 0
  %90 = or i32 %89, %CtlSysEFLAGS_0
  %91 = zext i1 %PF_0 to i32
  %92 = shl i32 %91, 2
  %93 = or i32 %92, %90
  %94 = zext i1 false to i32
  %95 = shl i32 %94, 4
  %96 = or i32 %95, %93
  %97 = zext i1 %ZF_0 to i32
  %98 = shl i32 %97, 6
  %99 = or i32 %98, %96
  %100 = zext i1 %SF_0 to i32
  %101 = shl i32 %100, 7
  %102 = or i32 %101, %99
  %103 = zext i1 false to i32
  %104 = shl i32 %103, 11
  %EFLAGS_1 = or i32 %104, %102
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 -61, i8* %CH
  store i8 -77, i8* %CL
  store i16 -15437, i16* %CX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i8 -22, i8* %DH
  store i8 0, i8* %DL
  store i16 -5632, i16* %DX
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 -1430928461, i32* %ECX
  store i32 -94967296, i32* %EDX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_23, i32* %EIP
  store i32 -94967296, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_23, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 123123123123, i64* %RCX
  store i64 4200000000, i64* %RDX
  store i64 %RIP_24, i64* %RIP
  store i64 4200000000, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  store i16 -5632, i16* %SI
  store i8 0, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  %105 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %105, <4 x float>* %XMM0
  %106 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %106, <4 x float>* %XMM1
  %107 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %107, <8 x float>* %YMM0
  %108 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %108, <8 x float>* %YMM1
  %109 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %109, <16 x float>* %ZMM0
  %110 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %110, <16 x float>* %ZMM1
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
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
  br label %bb_4003D0

exit_fn_4003D0:                                   ; preds = %bb_4003EB, %bb_4003F8
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

bb_4003D0:                                        ; preds = %entry_fn_4003D0
  %RIP_1 = add i64 4195280, 1
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
  %CC_A_0 = icmp ugt i64 6295592, 6295592
  %CC_AE_0 = icmp uge i64 6295592, 6295592
  %CC_B_0 = icmp ult i64 6295592, 6295592
  %CC_BE_0 = icmp ule i64 6295592, 6295592
  %CC_L_0 = icmp slt i64 6295592, 6295592
  %CC_LE_0 = icmp sle i64 6295592, 6295592
  %CC_G_0 = icmp sgt i64 6295592, 6295592
  %CC_GE_0 = icmp sge i64 6295592, 6295592
  %CC_E_0 = icmp eq i64 6295592, 6295592
  %CC_NE_0 = icmp ne i64 6295592, 6295592
  %11 = sub i64 6295592, 6295592
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295592, i64 6295592)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295592, i64 6295592)
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
  store i8 40, i8* %AL
  store i16 4136, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295592, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195320, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1016, i16* %IP
  store i64 6295592, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195320, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003F8, label %bb_4003E1

bb_4003E1:                                        ; preds = %bb_4003D0
  %RIP_8 = add i64 4195297, 5
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
  store i32 4195320, i32* %EIP
  store i16 1016, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195320, i64* %RIP
  br i1 %CC_E_09, label %bb_4003F8, label %bb_4003EB

bb_4003EB:                                        ; preds = %bb_4003E1
  %RIP_17 = add i64 4195307, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_4003D0

bb_4003F8:                                        ; preds = %bb_4003E1, %bb_4003D0
  %RIP_13 = add i64 4195320, 1
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
  br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define void @fn_400400(%regset* noalias nocapture) {
entry_fn_400400:
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
  br label %bb_400400

exit_fn_400400:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400400:                                        ; preds = %entry_fn_400400
  %RIP_1 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_13 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_22 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400400

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_18 = add i64 4195384, 1
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
  br label %exit_fn_400400
}

define void @fn_400440(%regset* noalias nocapture) {
entry_fn_400440:
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
  br label %bb_400440

exit_fn_400440:                                   ; preds = %bb_400460, %bb_400449
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

bb_400440:                                        ; preds = %entry_fn_400440
  %RIP_1 = add i64 4195392, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100193
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
  store i32 4195424, i32* %EIP
  store i16 1120, i16* %IP
  store i64 4195424, i64* %RIP
  br i1 %CC_NE_0, label %bb_400460, label %bb_400449

bb_400449:                                        ; preds = %bb_400440
  %RIP_5 = add i64 4195401, 1
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
  store i64 4195410, i64* %33
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
  call void @fn_4003D0(%regset* %0)
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
  %44 = add i64 %RIP_9, 2100175
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
  br label %exit_fn_400440

bb_400460:                                        ; preds = %bb_400440
  %RIP_14 = add i64 4195424, 1
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
  br label %exit_fn_400440
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

define void @fn_400470(%regset* noalias nocapture) {
entry_fn_400470:
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
  br label %bb_400470

exit_fn_400470:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400470:                                        ; preds = %entry_fn_400470
  %RIP_1 = add i64 4195440, 1
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
  store i32 4195328, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1024, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195328, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400400

bb_400400:                                        ; preds = %bb_400470
  %RIP_7 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_19 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_28 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400470

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_24 = add i64 4195384, 1
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
  br label %exit_fn_400470
}

define void @fn_400570(%regset* noalias nocapture) {
entry_fn_400570:
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
  br label %bb_400570

exit_fn_400570:                                   ; preds = %bb_400570
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400570:                                        ; preds = %entry_fn_400570
  %RIP_1 = add i64 4195696, 1
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
  br label %exit_fn_400570
}

define void @fn_400574(%regset* noalias nocapture) {
entry_fn_400574:
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
  br label %bb_400574

exit_fn_400574:                                   ; preds = %bb_400574
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400574:                                        ; preds = %entry_fn_400574
  %RIP_1 = add i64 4195700, 4
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
  br label %exit_fn_400574
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

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
  br label %bb_400500

exit_fn_400500:                                   ; preds = %bb_400556
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

bb_400500:                                        ; preds = %entry_fn_400500
  %RIP_1 = add i64 4195584, 2
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
  %R12_1 = add i64 %RIP_6, 2099518
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
  %RBP_1 = add i64 %RIP_8, 2099518
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
  store i64 4195633, i64* %29
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
  call void @fn_400370(%regset* %0)
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
  store i32 4195670, i32* %EIP
  store i16 1366, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195670, i64* %RIP
  br i1 %CC_E_0, label %bb_400556, label %bb_400536

bb_400536:                                        ; preds = %bb_400500
  %RIP_21 = add i64 4195638, 2
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
  br label %bb_400540

bb_400540:                                        ; preds = %bb_400540, %bb_400536
  %RIP_34 = add i64 4195648, 3
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
  store i64 4195661, i64* %126
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
  store i32 4195648, i32* %EIP
  store i16 1344, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195648, i64* %RIP
  br i1 %CC_NE_016, label %bb_400540, label %bb_400556

bb_400556:                                        ; preds = %bb_400540, %bb_400500
  %RIP_24 = add i64 4195670, 4
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
  br label %exit_fn_400500
}

define void @fn_400370(%regset* noalias nocapture) {
entry_fn_400370:
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
  br label %bb_400370

exit_fn_400370:                                   ; preds = %bb_400382
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

bb_400370:                                        ; preds = %entry_fn_400370
  %RIP_1 = add i64 4195184, 4
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
  %7 = add i64 %RIP_2, 2100349
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
  store i32 4195202, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 898, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195202, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400382, label %bb_400380

bb_400380:                                        ; preds = %bb_400370
  %RIP_7 = add i64 4195200, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195202, i64* %34
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
  br label %bb_400382

bb_400382:                                        ; preds = %bb_400380, %bb_400370
  %RIP_9 = add i64 4195202, 4
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
  br label %exit_fn_400370
}

define void @fn_4003C0(%regset* noalias nocapture) {
entry_fn_4003C0:
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
  br label %bb_4003C0

exit_fn_4003C0:                                   ; preds = %bb_4003C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_4003C0:                                        ; preds = %entry_fn_4003C0
  %RIP_1 = add i64 4195264, 1
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
  br label %exit_fn_4003C0
}

define void @fn_400390(%regset* noalias nocapture) {
entry_fn_400390:
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
  br label %bb_400390

exit_fn_400390:                                   ; preds = %bb_400390
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

bb_400390:                                        ; preds = %entry_fn_400390
  %RIP_1 = add i64 4195216, 2
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
  %22 = add i64 %RIP_11, 2100278
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195258, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 5, i8* %CH
  store i8 0, i8* %CL
  store i16 1280, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1152, i16* %DI
  store i8 -128, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195584, i32* %ECX
  store i32 4195456, i32* %EDI
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
  store i64 4195584, i64* %RCX
  store i64 4195456, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195696, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 112, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195696, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1392, i16* %R8W
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
  br label %exit_fn_400390
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
  %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
  %ZMM0 = alloca <16 x float>
  store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
  %2 = bitcast <16 x float> %ZMM0_init to i512
  %3 = trunc i512 %2 to i128
  %XMM0_init = bitcast i128 %3 to <4 x float>
  %XMM0 = alloca <4 x float>
  store <4 x float> %XMM0_init, <4 x float>* %XMM0
  %4 = bitcast <16 x float> %ZMM0_init to i512
  %5 = trunc i512 %4 to i256
  %YMM0_init = bitcast i256 %5 to <8 x float>
  %YMM0 = alloca <8 x float>
  store <8 x float> %YMM0_init, <8 x float>* %YMM0
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %11 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %11 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400480
  %12 = load i32, i32* %CtlSysEFLAGS
  store i32 %12, i32* %CtlSysEFLAGS_ptr
  %13 = load i32, i32* %EFLAGS
  store i32 %13, i32* %EFLAGS_ptr
  %14 = load i64, i64* %RAX
  store i64 %14, i64* %RAX_ptr
  %15 = load i64, i64* %RBP
  store i64 %15, i64* %RBP_ptr
  %16 = load i64, i64* %RCX
  store i64 %16, i64* %RCX_ptr
  %17 = load i64, i64* %RDX
  store i64 %17, i64* %RDX_ptr
  %18 = load i64, i64* %RIP
  store i64 %18, i64* %RIP_ptr
  %19 = load i64, i64* %RSI
  store i64 %19, i64* %RSI_ptr
  %20 = load i64, i64* %RSP
  store i64 %20, i64* %RSP_ptr
  %21 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %21, <16 x float>* %ZMM0_ptr
  %22 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %22, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %23 = sub i64 %RSP_0, 8
  %24 = inttoptr i64 %23 to i64*
  store i64 %RBP_0, i64* %24, align 1
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
  %RIP_3 = add i64 %RIP_2, 2
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %EAX_1 = xor i32 %EAX_0, %EAX_0
  %RAX_1 = zext i32 %EAX_1 to i64
  %AX_0 = trunc i32 %EAX_1 to i16
  %AL_0 = trunc i32 %EAX_1 to i8
  %25 = lshr i32 %EAX_1, 8
  %AH_0 = trunc i32 %25 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %26 = add i64 %RIP_4, 250
  %27 = inttoptr i64 %26 to double*
  %28 = load double, double* %27, align 1
  %29 = bitcast double %28 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %30 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %30 to i128
  %31 = zext i64 %29 to i128
  %32 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %31, %32
  %33 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %33 to i256
  %34 = zext i128 %XMM0_1 to i256
  %35 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %34, %35
  %36 = bitcast <16 x float> %ZMM0_0 to i512
  %37 = zext i128 %XMM0_1 to i512
  %38 = and i512 %36, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %37, %38
  %RIP_5 = add i64 %RIP_4, 8
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %39 = add i64 %RIP_5, 250
  %40 = inttoptr i64 %39 to float*
  %41 = load float, float* %40, align 1
  %42 = bitcast float %41 to i32
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %43 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %43 to i128
  %44 = zext i32 %42 to i128
  %45 = and i128 %XMM1_0, -4294967296
  %XMM1_1 = or i128 %44, %45
  %46 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %46 to i256
  %47 = zext i128 %XMM1_1 to i256
  %48 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %47, %48
  %49 = bitcast <16 x float> %ZMM1_0 to i512
  %50 = zext i128 %XMM1_1 to i512
  %51 = and i512 %49, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %50, %51
  %RIP_6 = add i64 %RIP_5, 10
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RDX_0 = load i64, i64* %RDX
  %RIP_8 = add i64 %RIP_7, 2
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_9 = add i64 %RIP_8, 7
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %52 = add i64 %RSP_1, -4
  %53 = inttoptr i64 %52 to i32*
  store i32 0, i32* %53, align 1
  %RIP_10 = add i64 %RIP_9, 6
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %54 = add i64 %RSP_1, -6
  %55 = inttoptr i64 %54 to i16*
  store i16 -32100, i16* %55, align 1
  %RIP_11 = add i64 %RIP_10, 6
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %56 = add i64 %RSP_1, -8
  %57 = inttoptr i64 %56 to i16*
  store i16 -104, i16* %57, align 1
  %RIP_12 = add i64 %RIP_11, 7
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %58 = add i64 %RSP_1, -12
  %59 = inttoptr i64 %58 to i32*
  store i32 -4967296, i32* %59, align 1
  %RIP_13 = add i64 %RIP_12, 7
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %60 = add i64 %RSP_1, -16
  %61 = inttoptr i64 %60 to i32*
  store i32 -2123123123, i32* %61, align 1
  %RIP_14 = add i64 %RIP_13, 8
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %62 = add i64 %RSP_1, -24
  %63 = inttoptr i64 %62 to i64*
  store i64 -2111111111, i64* %63, align 1
  %RIP_15 = add i64 %RIP_14, 4
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %64 = add i64 %RSP_1, -32
  %65 = inttoptr i64 %64 to i64*
  store i64 4200000000, i64* %65, align 1
  %RIP_16 = add i64 %RIP_15, 8
  %EIP_15 = trunc i64 %RIP_16 to i32
  %IP_15 = trunc i64 %RIP_16 to i16
  %66 = add i64 %RSP_1, -40
  %67 = inttoptr i64 %66 to i64*
  store i64 -123123123, i64* %67, align 1
  %RIP_17 = add i64 %RIP_16, 4
  %EIP_16 = trunc i64 %RIP_17 to i32
  %IP_16 = trunc i64 %RIP_17 to i16
  %68 = add i64 %RSP_1, -48
  %69 = inttoptr i64 %68 to i64*
  store i64 123123123123, i64* %69, align 1
  %RIP_18 = add i64 %RIP_17, 4
  %EIP_17 = trunc i64 %RIP_18 to i32
  %IP_17 = trunc i64 %RIP_18 to i16
  %70 = add i64 %RSP_1, -49
  %71 = inttoptr i64 %70 to i8*
  store i8 32, i8* %71, align 1
  %RIP_19 = add i64 %RIP_18, 4
  %EIP_18 = trunc i64 %RIP_19 to i32
  %IP_18 = trunc i64 %RIP_19 to i16
  %72 = add i64 %RSP_1, -50
  %73 = inttoptr i64 %72 to i8*
  store i8 33, i8* %73, align 1
  %RIP_20 = add i64 %RIP_19, 5
  %EIP_19 = trunc i64 %RIP_20 to i32
  %IP_19 = trunc i64 %RIP_20 to i16
  %74 = trunc i128 %XMM1_1 to i32
  %75 = bitcast i32 %74 to float
  %76 = add i64 %RSP_1, -56
  %77 = inttoptr i64 %76 to float*
  store float %75, float* %77, align 1
  %RIP_21 = add i64 %RIP_20, 5
  %EIP_20 = trunc i64 %RIP_21 to i32
  %IP_20 = trunc i64 %RIP_21 to i16
  %78 = trunc i128 %XMM0_1 to i64
  %79 = bitcast i64 %78 to double
  %80 = add i64 %RSP_1, -64
  %81 = inttoptr i64 %80 to double*
  store double %79, double* %81, align 1
  %RIP_22 = add i64 %RIP_21, 1
  %EIP_21 = trunc i64 %RIP_22 to i32
  %IP_21 = trunc i64 %RIP_22 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %82 = sub i64 %RSP_2, 8
  %83 = inttoptr i64 %82 to i64*
  %RBP_1 = load i64, i64* %83, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_23 = add i64 %RIP_22, 1
  %EIP_22 = trunc i64 %RIP_23 to i32
  %IP_22 = trunc i64 %RIP_23 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %84 = inttoptr i64 %RSP_2 to i64*
  %RIP_24 = load i64, i64* %84
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_23 = trunc i64 %RIP_24 to i32
  %IP_23 = trunc i64 %RIP_24 to i16
  %ZF_0 = icmp eq i32 %EAX_1, 0
  %SF_0 = icmp slt i32 %EAX_1, 0
  %85 = trunc i32 %EAX_1 to i8
  %86 = call i8 @llvm.ctpop.i8(i8 %85)
  %87 = trunc i8 %86 to i1
  %PF_0 = icmp eq i1 %87, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %88 = zext i1 false to i32
  %89 = shl i32 %88, 0
  %90 = or i32 %89, %CtlSysEFLAGS_0
  %91 = zext i1 %PF_0 to i32
  %92 = shl i32 %91, 2
  %93 = or i32 %92, %90
  %94 = zext i1 false to i32
  %95 = shl i32 %94, 4
  %96 = or i32 %95, %93
  %97 = zext i1 %ZF_0 to i32
  %98 = shl i32 %97, 6
  %99 = or i32 %98, %96
  %100 = zext i1 %SF_0 to i32
  %101 = shl i32 %100, 7
  %102 = or i32 %101, %99
  %103 = zext i1 false to i32
  %104 = shl i32 %103, 11
  %EFLAGS_1 = or i32 %104, %102
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 -61, i8* %CH
  store i8 -77, i8* %CL
  store i16 -15437, i16* %CX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i8 -22, i8* %DH
  store i8 0, i8* %DL
  store i16 -5632, i16* %DX
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 -1430928461, i32* %ECX
  store i32 -94967296, i32* %EDX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_23, i32* %EIP
  store i32 -94967296, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_23, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 123123123123, i64* %RCX
  store i64 4200000000, i64* %RDX
  store i64 %RIP_24, i64* %RIP
  store i64 4200000000, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  store i16 -5632, i16* %SI
  store i8 0, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  %105 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %105, <4 x float>* %XMM0
  %106 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %106, <4 x float>* %XMM1
  %107 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %107, <8 x float>* %YMM0
  %108 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %108, <8 x float>* %YMM1
  %109 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %109, <16 x float>* %ZMM0
  %110 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %110, <16 x float>* %ZMM1
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
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
  br label %bb_4003D0

exit_fn_4003D0:                                   ; preds = %bb_4003EB, %bb_4003F8
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

bb_4003D0:                                        ; preds = %entry_fn_4003D0
  %RIP_1 = add i64 4195280, 1
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
  %CC_A_0 = icmp ugt i64 6295592, 6295592
  %CC_AE_0 = icmp uge i64 6295592, 6295592
  %CC_B_0 = icmp ult i64 6295592, 6295592
  %CC_BE_0 = icmp ule i64 6295592, 6295592
  %CC_L_0 = icmp slt i64 6295592, 6295592
  %CC_LE_0 = icmp sle i64 6295592, 6295592
  %CC_G_0 = icmp sgt i64 6295592, 6295592
  %CC_GE_0 = icmp sge i64 6295592, 6295592
  %CC_E_0 = icmp eq i64 6295592, 6295592
  %CC_NE_0 = icmp ne i64 6295592, 6295592
  %11 = sub i64 6295592, 6295592
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295592, i64 6295592)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295592, i64 6295592)
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
  store i8 40, i8* %AL
  store i16 4136, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295592, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195320, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1016, i16* %IP
  store i64 6295592, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195320, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003F8, label %bb_4003E1

bb_4003E1:                                        ; preds = %bb_4003D0
  %RIP_8 = add i64 4195297, 5
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
  store i32 4195320, i32* %EIP
  store i16 1016, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195320, i64* %RIP
  br i1 %CC_E_09, label %bb_4003F8, label %bb_4003EB

bb_4003EB:                                        ; preds = %bb_4003E1
  %RIP_17 = add i64 4195307, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_4003D0

bb_4003F8:                                        ; preds = %bb_4003E1, %bb_4003D0
  %RIP_13 = add i64 4195320, 1
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
  br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define void @fn_400400(%regset* noalias nocapture) {
entry_fn_400400:
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
  br label %bb_400400

exit_fn_400400:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400400:                                        ; preds = %entry_fn_400400
  %RIP_1 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_13 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_22 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400400

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_18 = add i64 4195384, 1
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
  br label %exit_fn_400400
}

define void @fn_400440(%regset* noalias nocapture) {
entry_fn_400440:
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
  br label %bb_400440

exit_fn_400440:                                   ; preds = %bb_400460, %bb_400449
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

bb_400440:                                        ; preds = %entry_fn_400440
  %RIP_1 = add i64 4195392, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100193
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
  store i32 4195424, i32* %EIP
  store i16 1120, i16* %IP
  store i64 4195424, i64* %RIP
  br i1 %CC_NE_0, label %bb_400460, label %bb_400449

bb_400449:                                        ; preds = %bb_400440
  %RIP_5 = add i64 4195401, 1
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
  store i64 4195410, i64* %33
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
  call void @fn_4003D0(%regset* %0)
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
  %44 = add i64 %RIP_9, 2100175
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
  br label %exit_fn_400440

bb_400460:                                        ; preds = %bb_400440
  %RIP_14 = add i64 4195424, 1
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
  br label %exit_fn_400440
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

define void @fn_400470(%regset* noalias nocapture) {
entry_fn_400470:
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
  br label %bb_400470

exit_fn_400470:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400470:                                        ; preds = %entry_fn_400470
  %RIP_1 = add i64 4195440, 1
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
  store i32 4195328, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1024, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195328, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400400

bb_400400:                                        ; preds = %bb_400470
  %RIP_7 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_19 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_28 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400470

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_24 = add i64 4195384, 1
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
  br label %exit_fn_400470
}

define void @fn_400570(%regset* noalias nocapture) {
entry_fn_400570:
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
  br label %bb_400570

exit_fn_400570:                                   ; preds = %bb_400570
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400570:                                        ; preds = %entry_fn_400570
  %RIP_1 = add i64 4195696, 1
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
  br label %exit_fn_400570
}

define void @fn_400574(%regset* noalias nocapture) {
entry_fn_400574:
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
  br label %bb_400574

exit_fn_400574:                                   ; preds = %bb_400574
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400574:                                        ; preds = %entry_fn_400574
  %RIP_1 = add i64 4195700, 4
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
  br label %exit_fn_400574
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

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
  br label %bb_400500

exit_fn_400500:                                   ; preds = %bb_400556
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

bb_400500:                                        ; preds = %entry_fn_400500
  %RIP_1 = add i64 4195584, 2
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
  %R12_1 = add i64 %RIP_6, 2099518
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
  %RBP_1 = add i64 %RIP_8, 2099518
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
  store i64 4195633, i64* %29
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
  call void @fn_400370(%regset* %0)
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
  store i32 4195670, i32* %EIP
  store i16 1366, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195670, i64* %RIP
  br i1 %CC_E_0, label %bb_400556, label %bb_400536

bb_400536:                                        ; preds = %bb_400500
  %RIP_21 = add i64 4195638, 2
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
  br label %bb_400540

bb_400540:                                        ; preds = %bb_400540, %bb_400536
  %RIP_34 = add i64 4195648, 3
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
  store i64 4195661, i64* %126
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
  store i32 4195648, i32* %EIP
  store i16 1344, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195648, i64* %RIP
  br i1 %CC_NE_016, label %bb_400540, label %bb_400556

bb_400556:                                        ; preds = %bb_400540, %bb_400500
  %RIP_24 = add i64 4195670, 4
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
  br label %exit_fn_400500
}

define void @fn_400370(%regset* noalias nocapture) {
entry_fn_400370:
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
  br label %bb_400370

exit_fn_400370:                                   ; preds = %bb_400382
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

bb_400370:                                        ; preds = %entry_fn_400370
  %RIP_1 = add i64 4195184, 4
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
  %7 = add i64 %RIP_2, 2100349
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
  store i32 4195202, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 898, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195202, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400382, label %bb_400380

bb_400380:                                        ; preds = %bb_400370
  %RIP_7 = add i64 4195200, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195202, i64* %34
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
  br label %bb_400382

bb_400382:                                        ; preds = %bb_400380, %bb_400370
  %RIP_9 = add i64 4195202, 4
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
  br label %exit_fn_400370
}

define void @fn_4003C0(%regset* noalias nocapture) {
entry_fn_4003C0:
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
  br label %bb_4003C0

exit_fn_4003C0:                                   ; preds = %bb_4003C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_4003C0:                                        ; preds = %entry_fn_4003C0
  %RIP_1 = add i64 4195264, 1
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
  br label %exit_fn_4003C0
}

define void @fn_400390(%regset* noalias nocapture) {
entry_fn_400390:
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
  br label %bb_400390

exit_fn_400390:                                   ; preds = %bb_400390
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

bb_400390:                                        ; preds = %entry_fn_400390
  %RIP_1 = add i64 4195216, 2
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
  %22 = add i64 %RIP_11, 2100278
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195258, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 5, i8* %CH
  store i8 0, i8* %CL
  store i16 1280, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1152, i16* %DI
  store i8 -128, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195584, i32* %ECX
  store i32 4195456, i32* %EDI
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
  store i64 4195584, i64* %RCX
  store i64 4195456, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195696, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 112, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195696, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1392, i16* %R8W
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
  br label %exit_fn_400390
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %3 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %3 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
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
  %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
  %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
  %ZMM0 = alloca <16 x float>
  store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
  %4 = bitcast <16 x float> %ZMM0_init to i512
  %5 = trunc i512 %4 to i128
  %XMM0_init = bitcast i128 %5 to <4 x float>
  %XMM0 = alloca <4 x float>
  store <4 x float> %XMM0_init, <4 x float>* %XMM0
  %6 = bitcast <16 x float> %ZMM0_init to i512
  %7 = trunc i512 %6 to i256
  %YMM0_init = bitcast i256 %7 to <8 x float>
  %YMM0 = alloca <8 x float>
  store <8 x float> %YMM0_init, <8 x float>* %YMM0
  %ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
  %ZMM1_init = load <16 x float>, <16 x float>* %ZMM1_ptr
  %ZMM1 = alloca <16 x float>
  store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
  %8 = bitcast <16 x float> %ZMM1_init to i512
  %9 = trunc i512 %8 to i128
  %XMM1_init = bitcast i128 %9 to <4 x float>
  %XMM1 = alloca <4 x float>
  store <4 x float> %XMM1_init, <4 x float>* %XMM1
  %10 = bitcast <16 x float> %ZMM1_init to i512
  %11 = trunc i512 %10 to i256
  %YMM1_init = bitcast i256 %11 to <8 x float>
  %YMM1 = alloca <8 x float>
  store <8 x float> %YMM1_init, <8 x float>* %YMM1
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400480
  %12 = load i32, i32* %CtlSysEFLAGS
  store i32 %12, i32* %CtlSysEFLAGS_ptr
  %13 = load i32, i32* %EFLAGS
  store i32 %13, i32* %EFLAGS_ptr
  %14 = load i64, i64* %RAX
  store i64 %14, i64* %RAX_ptr
  %15 = load i64, i64* %RBP
  store i64 %15, i64* %RBP_ptr
  %16 = load i64, i64* %RCX
  store i64 %16, i64* %RCX_ptr
  %17 = load i64, i64* %RDI
  store i64 %17, i64* %RDI_ptr
  %18 = load i64, i64* %RDX
  store i64 %18, i64* %RDX_ptr
  %19 = load i64, i64* %RIP
  store i64 %19, i64* %RIP_ptr
  %20 = load i64, i64* %RSI
  store i64 %20, i64* %RSI_ptr
  %21 = load i64, i64* %RSP
  store i64 %21, i64* %RSP_ptr
  %22 = load i64, i64* %R8
  store i64 %22, i64* %R8_ptr
  %23 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %23, <16 x float>* %ZMM0_ptr
  %24 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %24, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %25 = sub i64 %RSP_0, 8
  %26 = inttoptr i64 %25 to i64*
  store i64 %RBP_0, i64* %26, align 1
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
  %RIP_3 = add i64 %RIP_2, 2
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %EAX_1 = xor i32 %EAX_0, %EAX_0
  %RAX_1 = zext i32 %EAX_1 to i64
  %AX_0 = trunc i32 %EAX_1 to i16
  %AL_0 = trunc i32 %EAX_1 to i8
  %27 = lshr i32 %EAX_1, 8
  %AH_0 = trunc i32 %27 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 5
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %RCX_0 = load i64, i64* %RCX
  %RIP_5 = add i64 %RIP_4, 2
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %RDX_0 = load i64, i64* %RDX
  %RIP_6 = add i64 %RIP_5, 5
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %RIP_7 = add i64 %RIP_6, 2
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %RSI_0 = load i64, i64* %RSI
  %RIP_8 = add i64 %RIP_7, 8
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %28 = add i64 %RIP_8, 252
  %29 = inttoptr i64 %28 to double*
  %30 = load double, double* %29, align 1
  %31 = bitcast double %30 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %32 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %32 to i128
  %33 = zext i64 %31 to i128
  %34 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %33, %34
  %35 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %35 to i256
  %36 = zext i128 %XMM0_1 to i256
  %37 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %36, %37
  %38 = bitcast <16 x float> %ZMM0_0 to i512
  %39 = zext i128 %XMM0_1 to i512
  %40 = and i512 %38, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %39, %40
  %RIP_9 = add i64 %RIP_8, 8
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %41 = add i64 %RIP_9, 252
  %42 = inttoptr i64 %41 to float*
  %43 = load float, float* %42, align 1
  %44 = bitcast float %43 to i32
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %45 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %45 to i128
  %46 = zext i32 %44 to i128
  %47 = and i128 %XMM1_0, -4294967296
  %XMM1_1 = or i128 %46, %47
  %48 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %48 to i256
  %49 = zext i128 %XMM1_1 to i256
  %50 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %49, %50
  %51 = bitcast <16 x float> %ZMM1_0 to i512
  %52 = zext i128 %XMM1_1 to i512
  %53 = and i512 %51, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %52, %53
  %RIP_10 = add i64 %RIP_9, 10
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %RIP_11 = add i64 %RIP_10, 5
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %RIP_12 = add i64 %RIP_11, 3
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %R8_0 = load i64, i64* %R8
  %RIP_13 = add i64 %RIP_12, 7
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %54 = add i64 %RSP_1, -4
  %55 = inttoptr i64 %54 to i32*
  store i32 0, i32* %55, align 1
  %RIP_14 = add i64 %RIP_13, 6
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %56 = add i64 %RSP_1, -6
  %57 = inttoptr i64 %56 to i16*
  store i16 -32100, i16* %57, align 1
  %RIP_15 = add i64 %RIP_14, 6
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %58 = add i64 %RSP_1, -8
  %59 = inttoptr i64 %58 to i16*
  store i16 -104, i16* %59, align 1
  %RIP_16 = add i64 %RIP_15, 7
  %EIP_15 = trunc i64 %RIP_16 to i32
  %IP_15 = trunc i64 %RIP_16 to i16
  %60 = add i64 %RSP_1, -12
  %61 = inttoptr i64 %60 to i32*
  store i32 -4967296, i32* %61, align 1
  %RIP_17 = add i64 %RIP_16, 7
  %EIP_16 = trunc i64 %RIP_17 to i32
  %IP_16 = trunc i64 %RIP_17 to i16
  %62 = add i64 %RSP_1, -16
  %63 = inttoptr i64 %62 to i32*
  store i32 -2123123123, i32* %63, align 1
  %RIP_18 = add i64 %RIP_17, 8
  %EIP_17 = trunc i64 %RIP_18 to i32
  %IP_17 = trunc i64 %RIP_18 to i16
  %64 = add i64 %RSP_1, -24
  %65 = inttoptr i64 %64 to i64*
  store i64 -2111111111, i64* %65, align 1
  %RIP_19 = add i64 %RIP_18, 4
  %EIP_18 = trunc i64 %RIP_19 to i32
  %IP_18 = trunc i64 %RIP_19 to i16
  %66 = add i64 %RSP_1, -32
  %67 = inttoptr i64 %66 to i64*
  store i64 4200000000, i64* %67, align 1
  %RIP_20 = add i64 %RIP_19, 8
  %EIP_19 = trunc i64 %RIP_20 to i32
  %IP_19 = trunc i64 %RIP_20 to i16
  %68 = add i64 %RSP_1, -40
  %69 = inttoptr i64 %68 to i64*
  store i64 -123123123, i64* %69, align 1
  %RIP_21 = add i64 %RIP_20, 4
  %EIP_20 = trunc i64 %RIP_21 to i32
  %IP_20 = trunc i64 %RIP_21 to i16
  %70 = add i64 %RSP_1, -48
  %71 = inttoptr i64 %70 to i64*
  store i64 123123123123, i64* %71, align 1
  %RIP_22 = add i64 %RIP_21, 4
  %EIP_21 = trunc i64 %RIP_22 to i32
  %IP_21 = trunc i64 %RIP_22 to i16
  %72 = add i64 %RSP_1, -49
  %73 = inttoptr i64 %72 to i8*
  store i8 32, i8* %73, align 1
  %RIP_23 = add i64 %RIP_22, 4
  %EIP_22 = trunc i64 %RIP_23 to i32
  %IP_22 = trunc i64 %RIP_23 to i16
  %74 = add i64 %RSP_1, -50
  %75 = inttoptr i64 %74 to i8*
  store i8 33, i8* %75, align 1
  %RIP_24 = add i64 %RIP_23, 5
  %EIP_23 = trunc i64 %RIP_24 to i32
  %IP_23 = trunc i64 %RIP_24 to i16
  %76 = trunc i128 %XMM1_1 to i32
  %77 = bitcast i32 %76 to float
  %78 = add i64 %RSP_1, -56
  %79 = inttoptr i64 %78 to float*
  store float %77, float* %79, align 1
  %RIP_25 = add i64 %RIP_24, 5
  %EIP_24 = trunc i64 %RIP_25 to i32
  %IP_24 = trunc i64 %RIP_25 to i16
  %80 = trunc i128 %XMM0_1 to i64
  %81 = bitcast i64 %80 to double
  %82 = add i64 %RSP_1, -64
  %83 = inttoptr i64 %82 to double*
  store double %81, double* %83, align 1
  %RIP_26 = add i64 %RIP_25, 4
  %EIP_25 = trunc i64 %RIP_26 to i32
  %IP_25 = trunc i64 %RIP_26 to i16
  %84 = add i64 %RSP_1, -72
  %85 = inttoptr i64 %84 to i64*
  store i64 123, i64* %85, align 1
  %RIP_27 = add i64 %RIP_26, 4
  %EIP_26 = trunc i64 %RIP_27 to i32
  %IP_26 = trunc i64 %RIP_27 to i16
  %86 = add i64 %RSP_1, -80
  %87 = inttoptr i64 %86 to i64*
  store i64 234, i64* %87, align 1
  %RIP_28 = add i64 %RIP_27, 1
  %EIP_27 = trunc i64 %RIP_28 to i32
  %IP_27 = trunc i64 %RIP_28 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %88 = sub i64 %RSP_2, 8
  %89 = inttoptr i64 %88 to i64*
  %RBP_1 = load i64, i64* %89, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_29 = add i64 %RIP_28, 1
  %EIP_28 = trunc i64 %RIP_29 to i32
  %IP_28 = trunc i64 %RIP_29 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %90 = inttoptr i64 %RSP_2 to i64*
  %RIP_30 = load i64, i64* %90
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_29 = trunc i64 %RIP_30 to i32
  %IP_29 = trunc i64 %RIP_30 to i16
  %ZF_0 = icmp eq i32 %EAX_1, 0
  %SF_0 = icmp slt i32 %EAX_1, 0
  %91 = trunc i32 %EAX_1 to i8
  %92 = call i8 @llvm.ctpop.i8(i8 %91)
  %93 = trunc i8 %92 to i1
  %PF_0 = icmp eq i1 %93, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %94 = zext i1 false to i32
  %95 = shl i32 %94, 0
  %96 = or i32 %95, %CtlSysEFLAGS_0
  %97 = zext i1 %PF_0 to i32
  %98 = shl i32 %97, 2
  %99 = or i32 %98, %96
  %100 = zext i1 false to i32
  %101 = shl i32 %100, 4
  %102 = or i32 %101, %99
  %103 = zext i1 %ZF_0 to i32
  %104 = shl i32 %103, 6
  %105 = or i32 %104, %102
  %106 = zext i1 %SF_0 to i32
  %107 = shl i32 %106, 7
  %108 = or i32 %107, %105
  %109 = zext i1 false to i32
  %110 = shl i32 %109, 11
  %EFLAGS_1 = or i32 %110, %108
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 -22, i8* %CH
  store i8 0, i8* %CL
  store i16 -5632, i16* %CX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i8 0, i8* %DH
  store i16 -15437, i16* %DI
  store i8 -77, i8* %DIL
  store i8 -22, i8* %DL
  store i16 234, i16* %DX
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 -94967296, i32* %ECX
  store i32 -1430928461, i32* %EDI
  store i32 234, i32* %EDX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_29, i32* %EIP
  store i32 123, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_29, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 4200000000, i64* %RCX
  store i64 123123123123, i64* %RDI
  store i64 234, i64* %RDX
  store i64 %RIP_30, i64* %RIP
  store i64 123, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  store i16 123, i16* %SI
  store i8 123, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  store i64 4200000000, i64* %R8
  %111 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %111, <4 x float>* %XMM0
  %112 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %112, <4 x float>* %XMM1
  %113 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %113, <8 x float>* %YMM0
  %114 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %114, <8 x float>* %YMM1
  %115 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %115, <16 x float>* %ZMM0
  %116 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %116, <16 x float>* %ZMM1
  store i8 0, i8* %R8B
  store i32 -94967296, i32* %R8D
  store i16 -5632, i16* %R8W
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
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
  br label %bb_4003D0

exit_fn_4003D0:                                   ; preds = %bb_4003EB, %bb_4003F8
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

bb_4003D0:                                        ; preds = %entry_fn_4003D0
  %RIP_1 = add i64 4195280, 1
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
  %CC_A_0 = icmp ugt i64 6295592, 6295592
  %CC_AE_0 = icmp uge i64 6295592, 6295592
  %CC_B_0 = icmp ult i64 6295592, 6295592
  %CC_BE_0 = icmp ule i64 6295592, 6295592
  %CC_L_0 = icmp slt i64 6295592, 6295592
  %CC_LE_0 = icmp sle i64 6295592, 6295592
  %CC_G_0 = icmp sgt i64 6295592, 6295592
  %CC_GE_0 = icmp sge i64 6295592, 6295592
  %CC_E_0 = icmp eq i64 6295592, 6295592
  %CC_NE_0 = icmp ne i64 6295592, 6295592
  %11 = sub i64 6295592, 6295592
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295592, i64 6295592)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295592, i64 6295592)
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
  store i8 40, i8* %AL
  store i16 4136, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295592, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195320, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1016, i16* %IP
  store i64 6295592, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195320, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003F8, label %bb_4003E1

bb_4003E1:                                        ; preds = %bb_4003D0
  %RIP_8 = add i64 4195297, 5
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
  store i32 4195320, i32* %EIP
  store i16 1016, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195320, i64* %RIP
  br i1 %CC_E_09, label %bb_4003F8, label %bb_4003EB

bb_4003EB:                                        ; preds = %bb_4003E1
  %RIP_17 = add i64 4195307, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_4003D0

bb_4003F8:                                        ; preds = %bb_4003E1, %bb_4003D0
  %RIP_13 = add i64 4195320, 1
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
  br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define void @fn_400400(%regset* noalias nocapture) {
entry_fn_400400:
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
  br label %bb_400400

exit_fn_400400:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400400:                                        ; preds = %entry_fn_400400
  %RIP_1 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_13 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_22 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400400

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_18 = add i64 4195384, 1
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
  br label %exit_fn_400400
}

define void @fn_400440(%regset* noalias nocapture) {
entry_fn_400440:
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
  br label %bb_400440

exit_fn_400440:                                   ; preds = %bb_400460, %bb_400449
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

bb_400440:                                        ; preds = %entry_fn_400440
  %RIP_1 = add i64 4195392, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100193
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
  store i32 4195424, i32* %EIP
  store i16 1120, i16* %IP
  store i64 4195424, i64* %RIP
  br i1 %CC_NE_0, label %bb_400460, label %bb_400449

bb_400449:                                        ; preds = %bb_400440
  %RIP_5 = add i64 4195401, 1
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
  store i64 4195410, i64* %33
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
  call void @fn_4003D0(%regset* %0)
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
  %44 = add i64 %RIP_9, 2100175
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
  br label %exit_fn_400440

bb_400460:                                        ; preds = %bb_400440
  %RIP_14 = add i64 4195424, 1
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
  br label %exit_fn_400440
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

define void @fn_400470(%regset* noalias nocapture) {
entry_fn_400470:
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
  br label %bb_400470

exit_fn_400470:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400470:                                        ; preds = %entry_fn_400470
  %RIP_1 = add i64 4195440, 1
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
  store i32 4195328, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1024, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195328, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400400

bb_400400:                                        ; preds = %bb_400470
  %RIP_7 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_19 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_28 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400470

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_24 = add i64 4195384, 1
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
  br label %exit_fn_400470
}

define void @fn_400580(%regset* noalias nocapture) {
entry_fn_400580:
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
  br label %bb_400580

exit_fn_400580:                                   ; preds = %bb_400580
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400580:                                        ; preds = %entry_fn_400580
  %RIP_1 = add i64 4195712, 1
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
  br label %exit_fn_400580
}

define void @fn_400584(%regset* noalias nocapture) {
entry_fn_400584:
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
  br label %bb_400584

exit_fn_400584:                                   ; preds = %bb_400584
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400584:                                        ; preds = %entry_fn_400584
  %RIP_1 = add i64 4195716, 4
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
  br label %exit_fn_400584
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

define void @fn_400510(%regset* noalias nocapture) {
entry_fn_400510:
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
  br label %bb_400510

exit_fn_400510:                                   ; preds = %bb_400566
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

bb_400510:                                        ; preds = %entry_fn_400510
  %RIP_1 = add i64 4195600, 2
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
  %R12_1 = add i64 %RIP_6, 2099502
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
  %RBP_1 = add i64 %RIP_8, 2099502
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
  store i64 4195649, i64* %29
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
  call void @fn_400370(%regset* %0)
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
  store i32 4195686, i32* %EIP
  store i16 1382, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195686, i64* %RIP
  br i1 %CC_E_0, label %bb_400566, label %bb_400546

bb_400546:                                        ; preds = %bb_400510
  %RIP_21 = add i64 4195654, 2
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
  br label %bb_400550

bb_400550:                                        ; preds = %bb_400550, %bb_400546
  %RIP_34 = add i64 4195664, 3
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
  store i64 4195677, i64* %126
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
  store i32 4195664, i32* %EIP
  store i16 1360, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195664, i64* %RIP
  br i1 %CC_NE_016, label %bb_400550, label %bb_400566

bb_400566:                                        ; preds = %bb_400550, %bb_400510
  %RIP_24 = add i64 4195686, 4
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
  br label %exit_fn_400510
}

define void @fn_400370(%regset* noalias nocapture) {
entry_fn_400370:
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
  br label %bb_400370

exit_fn_400370:                                   ; preds = %bb_400382
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

bb_400370:                                        ; preds = %entry_fn_400370
  %RIP_1 = add i64 4195184, 4
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
  %7 = add i64 %RIP_2, 2100349
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
  store i32 4195202, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 898, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195202, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400382, label %bb_400380

bb_400380:                                        ; preds = %bb_400370
  %RIP_7 = add i64 4195200, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195202, i64* %34
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
  br label %bb_400382

bb_400382:                                        ; preds = %bb_400380, %bb_400370
  %RIP_9 = add i64 4195202, 4
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
  br label %exit_fn_400370
}

define void @fn_4003C0(%regset* noalias nocapture) {
entry_fn_4003C0:
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
  br label %bb_4003C0

exit_fn_4003C0:                                   ; preds = %bb_4003C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_4003C0:                                        ; preds = %entry_fn_4003C0
  %RIP_1 = add i64 4195264, 1
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
  br label %exit_fn_4003C0
}

define void @fn_400390(%regset* noalias nocapture) {
entry_fn_400390:
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
  br label %bb_400390

exit_fn_400390:                                   ; preds = %bb_400390
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

bb_400390:                                        ; preds = %entry_fn_400390
  %RIP_1 = add i64 4195216, 2
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
  %22 = add i64 %RIP_11, 2100278
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195258, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 5, i8* %CH
  store i8 16, i8* %CL
  store i16 1296, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1152, i16* %DI
  store i8 -128, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195600, i32* %ECX
  store i32 4195456, i32* %EDI
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
  store i64 4195600, i64* %RCX
  store i64 4195456, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195712, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 -128, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195712, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1408, i16* %R8W
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
  br label %exit_fn_400390
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
; ModuleID = 'dct module #0'
source_filename = "dct module #0"

%regset = type { i16, i32, i16, i32, i16, i16, i16, i16, i64, i64, i64, i64, i64, i64, i64, i64, i64, i16, <2 x i64>, <2 x i64>, <2 x i64>, <2 x i64>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i32, i32, i32, i32, i32, i32, i32, i32, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, <64 x i1>, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, x86_fp80, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float>, <16 x float> }

define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
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
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
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
  %RDX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 13
  %RDX_init = load i64, i64* %RDX_ptr
  %RDX = alloca i64
  store i64 %RDX_init, i64* %RDX
  %EDX_init = trunc i64 %RDX_init to i32
  %EDX = alloca i32
  store i32 %EDX_init, i32* %EDX
  %DX_init = trunc i64 %RDX_init to i16
  %DX = alloca i16
  store i16 %DX_init, i16* %DX
  %DL_init = trunc i64 %RDX_init to i8
  %DL = alloca i8
  store i8 %DL_init, i8* %DL
  %3 = lshr i64 %RDX_init, 8
  %DH_init = trunc i64 %3 to i8
  %DH = alloca i8
  store i8 %DH_init, i8* %DH
  %ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
  %ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
  %ZMM0 = alloca <16 x float>
  store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
  %4 = bitcast <16 x float> %ZMM0_init to i512
  %5 = trunc i512 %4 to i128
  %XMM0_init = bitcast i128 %5 to <4 x float>
  %XMM0 = alloca <4 x float>
  store <4 x float> %XMM0_init, <4 x float>* %XMM0
  %6 = bitcast <16 x float> %ZMM0_init to i512
  %7 = trunc i512 %6 to i256
  %YMM0_init = bitcast i256 %7 to <8 x float>
  %YMM0 = alloca <8 x float>
  store <8 x float> %YMM0_init, <8 x float>* %YMM0
  %ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
  %ZMM1_init = load <16 x float>, <16 x float>* %ZMM1_ptr
  %ZMM1 = alloca <16 x float>
  store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
  %8 = bitcast <16 x float> %ZMM1_init to i512
  %9 = trunc i512 %8 to i128
  %XMM1_init = bitcast i128 %9 to <4 x float>
  %XMM1 = alloca <4 x float>
  store <4 x float> %XMM1_init, <4 x float>* %XMM1
  %10 = bitcast <16 x float> %ZMM1_init to i512
  %11 = trunc i512 %10 to i256
  %YMM1_init = bitcast i256 %11 to <8 x float>
  %YMM1 = alloca <8 x float>
  store <8 x float> %YMM1_init, <8 x float>* %YMM1
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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400480
  %12 = load i32, i32* %CtlSysEFLAGS
  store i32 %12, i32* %CtlSysEFLAGS_ptr
  %13 = load i32, i32* %EFLAGS
  store i32 %13, i32* %EFLAGS_ptr
  %14 = load i64, i64* %RAX
  store i64 %14, i64* %RAX_ptr
  %15 = load i64, i64* %RBP
  store i64 %15, i64* %RBP_ptr
  %16 = load i64, i64* %RCX
  store i64 %16, i64* %RCX_ptr
  %17 = load i64, i64* %RDI
  store i64 %17, i64* %RDI_ptr
  %18 = load i64, i64* %RDX
  store i64 %18, i64* %RDX_ptr
  %19 = load i64, i64* %RIP
  store i64 %19, i64* %RIP_ptr
  %20 = load i64, i64* %RSI
  store i64 %20, i64* %RSI_ptr
  %21 = load i64, i64* %RSP
  store i64 %21, i64* %RSP_ptr
  %22 = load i64, i64* %R8
  store i64 %22, i64* %R8_ptr
  %23 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %23, <16 x float>* %ZMM0_ptr
  %24 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %24, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %25 = sub i64 %RSP_0, 8
  %26 = inttoptr i64 %25 to i64*
  store i64 %RBP_0, i64* %26, align 1
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
  %RIP_3 = add i64 %RIP_2, 2
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %RAX_0 = load i64, i64* %RAX
  %EAX_0 = trunc i64 %RAX_0 to i32
  %EAX_1 = xor i32 %EAX_0, %EAX_0
  %RAX_1 = zext i32 %EAX_1 to i64
  %AX_0 = trunc i32 %EAX_1 to i16
  %AL_0 = trunc i32 %EAX_1 to i8
  %27 = lshr i32 %EAX_1, 8
  %AH_0 = trunc i32 %27 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_4 = add i64 %RIP_3, 4
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %RCX_0 = add i64 %RSP_1, -64
  %ECX_0 = trunc i64 %RCX_0 to i32
  %CX_0 = trunc i64 %RCX_0 to i16
  %CL_0 = trunc i64 %RCX_0 to i8
  %28 = lshr i64 %RCX_0, 8
  %CH_0 = trunc i64 %28 to i8
  %RIP_5 = add i64 %RIP_4, 4
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %RDX_0 = add i64 %RSP_1, -16
  %EDX_0 = trunc i64 %RDX_0 to i32
  %DX_0 = trunc i64 %RDX_0 to i16
  %DL_0 = trunc i64 %RDX_0 to i8
  %29 = lshr i64 %RDX_0, 8
  %DH_0 = trunc i64 %29 to i8
  %RIP_6 = add i64 %RIP_5, 8
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %30 = add i64 %RIP_6, 258
  %31 = inttoptr i64 %30 to double*
  %32 = load double, double* %31, align 1
  %33 = bitcast double %32 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %34 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %34 to i128
  %35 = zext i64 %33 to i128
  %36 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %35, %36
  %37 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %37 to i256
  %38 = zext i128 %XMM0_1 to i256
  %39 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %38, %39
  %40 = bitcast <16 x float> %ZMM0_0 to i512
  %41 = zext i128 %XMM0_1 to i512
  %42 = and i512 %40, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %41, %42
  %RIP_7 = add i64 %RIP_6, 8
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %43 = add i64 %RIP_7, 258
  %44 = inttoptr i64 %43 to float*
  %45 = load float, float* %44, align 1
  %46 = bitcast float %45 to i32
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %47 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %47 to i128
  %48 = zext i32 %46 to i128
  %49 = and i128 %XMM1_0, -4294967296
  %XMM1_1 = or i128 %48, %49
  %50 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %50 to i256
  %51 = zext i128 %XMM1_1 to i256
  %52 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %51, %52
  %53 = bitcast <16 x float> %ZMM1_0 to i512
  %54 = zext i128 %XMM1_1 to i512
  %55 = and i512 %53, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %54, %55
  %RIP_8 = add i64 %RIP_7, 10
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %RIP_9 = add i64 %RIP_8, 5
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %RDI_0 = load i64, i64* %RDI
  %RIP_10 = add i64 %RIP_9, 3
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %R8_0 = load i64, i64* %R8
  %RIP_11 = add i64 %RIP_10, 7
  %EIP_10 = trunc i64 %RIP_11 to i32
  %IP_10 = trunc i64 %RIP_11 to i16
  %56 = add i64 %RSP_1, -4
  %57 = inttoptr i64 %56 to i32*
  store i32 0, i32* %57, align 1
  %RIP_12 = add i64 %RIP_11, 6
  %EIP_11 = trunc i64 %RIP_12 to i32
  %IP_11 = trunc i64 %RIP_12 to i16
  %58 = add i64 %RSP_1, -6
  %59 = inttoptr i64 %58 to i16*
  store i16 -32100, i16* %59, align 1
  %RIP_13 = add i64 %RIP_12, 6
  %EIP_12 = trunc i64 %RIP_13 to i32
  %IP_12 = trunc i64 %RIP_13 to i16
  %60 = add i64 %RSP_1, -8
  %61 = inttoptr i64 %60 to i16*
  store i16 -104, i16* %61, align 1
  %RIP_14 = add i64 %RIP_13, 7
  %EIP_13 = trunc i64 %RIP_14 to i32
  %IP_13 = trunc i64 %RIP_14 to i16
  %62 = add i64 %RSP_1, -12
  %63 = inttoptr i64 %62 to i32*
  store i32 -4967296, i32* %63, align 1
  %RIP_15 = add i64 %RIP_14, 7
  %EIP_14 = trunc i64 %RIP_15 to i32
  %IP_14 = trunc i64 %RIP_15 to i16
  %64 = add i64 %RSP_1, -16
  %65 = inttoptr i64 %64 to i32*
  store i32 -2123123123, i32* %65, align 1
  %RIP_16 = add i64 %RIP_15, 8
  %EIP_15 = trunc i64 %RIP_16 to i32
  %IP_15 = trunc i64 %RIP_16 to i16
  %66 = add i64 %RSP_1, -24
  %67 = inttoptr i64 %66 to i64*
  store i64 -2111111111, i64* %67, align 1
  %RIP_17 = add i64 %RIP_16, 4
  %EIP_16 = trunc i64 %RIP_17 to i32
  %IP_16 = trunc i64 %RIP_17 to i16
  %68 = add i64 %RSP_1, -32
  %69 = inttoptr i64 %68 to i64*
  store i64 4200000000, i64* %69, align 1
  %RIP_18 = add i64 %RIP_17, 8
  %EIP_17 = trunc i64 %RIP_18 to i32
  %IP_17 = trunc i64 %RIP_18 to i16
  %70 = add i64 %RSP_1, -40
  %71 = inttoptr i64 %70 to i64*
  store i64 -123123123, i64* %71, align 1
  %RIP_19 = add i64 %RIP_18, 4
  %EIP_18 = trunc i64 %RIP_19 to i32
  %IP_18 = trunc i64 %RIP_19 to i16
  %72 = add i64 %RSP_1, -48
  %73 = inttoptr i64 %72 to i64*
  store i64 123123123123, i64* %73, align 1
  %RIP_20 = add i64 %RIP_19, 4
  %EIP_19 = trunc i64 %RIP_20 to i32
  %IP_19 = trunc i64 %RIP_20 to i16
  %74 = add i64 %RSP_1, -49
  %75 = inttoptr i64 %74 to i8*
  store i8 32, i8* %75, align 1
  %RIP_21 = add i64 %RIP_20, 4
  %EIP_20 = trunc i64 %RIP_21 to i32
  %IP_20 = trunc i64 %RIP_21 to i16
  %76 = add i64 %RSP_1, -50
  %77 = inttoptr i64 %76 to i8*
  store i8 33, i8* %77, align 1
  %RIP_22 = add i64 %RIP_21, 5
  %EIP_21 = trunc i64 %RIP_22 to i32
  %IP_21 = trunc i64 %RIP_22 to i16
  %78 = trunc i128 %XMM1_1 to i32
  %79 = bitcast i32 %78 to float
  %80 = add i64 %RSP_1, -56
  %81 = inttoptr i64 %80 to float*
  store float %79, float* %81, align 1
  %RIP_23 = add i64 %RIP_22, 5
  %EIP_22 = trunc i64 %RIP_23 to i32
  %IP_22 = trunc i64 %RIP_23 to i16
  %82 = trunc i128 %XMM0_1 to i64
  %83 = bitcast i64 %82 to double
  %84 = add i64 %RSP_1, -64
  %85 = inttoptr i64 %84 to double*
  store double %83, double* %85, align 1
  %RIP_24 = add i64 %RIP_23, 4
  %EIP_23 = trunc i64 %RIP_24 to i32
  %IP_23 = trunc i64 %RIP_24 to i16
  %86 = add i64 %RSP_1, -72
  %87 = inttoptr i64 %86 to i64*
  store i64 %RDX_0, i64* %87, align 1
  %RIP_25 = add i64 %RIP_24, 4
  %EIP_24 = trunc i64 %RIP_25 to i32
  %IP_24 = trunc i64 %RIP_25 to i16
  %88 = add i64 %RSP_1, -80
  %89 = inttoptr i64 %88 to i64*
  store i64 %RCX_0, i64* %89, align 1
  %RIP_26 = add i64 %RIP_25, 1
  %EIP_25 = trunc i64 %RIP_26 to i32
  %IP_25 = trunc i64 %RIP_26 to i16
  %RSP_2 = add i64 %RSP_1, 8
  %ESP_1 = trunc i64 %RSP_2 to i32
  %SP_1 = trunc i64 %RSP_2 to i16
  %SPL_1 = trunc i64 %RSP_2 to i8
  %90 = sub i64 %RSP_2, 8
  %91 = inttoptr i64 %90 to i64*
  %RBP_1 = load i64, i64* %91, align 1
  %EBP_1 = trunc i64 %RBP_1 to i32
  %BP_1 = trunc i64 %RBP_1 to i16
  %BPL_1 = trunc i64 %RBP_1 to i8
  %RIP_27 = add i64 %RIP_26, 1
  %EIP_26 = trunc i64 %RIP_27 to i32
  %IP_26 = trunc i64 %RIP_27 to i16
  %RSP_3 = add i64 %RSP_2, 8
  %92 = inttoptr i64 %RSP_2 to i64*
  %RIP_28 = load i64, i64* %92
  %ESP_2 = trunc i64 %RSP_3 to i32
  %SP_2 = trunc i64 %RSP_3 to i16
  %SPL_2 = trunc i64 %RSP_3 to i8
  %EIP_27 = trunc i64 %RIP_28 to i32
  %IP_27 = trunc i64 %RIP_28 to i16
  %ZF_0 = icmp eq i32 %EAX_1, 0
  %SF_0 = icmp slt i32 %EAX_1, 0
  %93 = trunc i32 %EAX_1 to i8
  %94 = call i8 @llvm.ctpop.i8(i8 %93)
  %95 = trunc i8 %94 to i1
  %PF_0 = icmp eq i1 %95, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %96 = zext i1 false to i32
  %97 = shl i32 %96, 0
  %98 = or i32 %97, %CtlSysEFLAGS_0
  %99 = zext i1 %PF_0 to i32
  %100 = shl i32 %99, 2
  %101 = or i32 %100, %98
  %102 = zext i1 false to i32
  %103 = shl i32 %102, 4
  %104 = or i32 %103, %101
  %105 = zext i1 %ZF_0 to i32
  %106 = shl i32 %105, 6
  %107 = or i32 %106, %104
  %108 = zext i1 %SF_0 to i32
  %109 = shl i32 %108, 7
  %110 = or i32 %109, %107
  %111 = zext i1 false to i32
  %112 = shl i32 %111, 11
  %EFLAGS_1 = or i32 %112, %110
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i8 %CH_0, i8* %CH
  store i8 %CL_0, i8* %CL
  store i16 %CX_0, i16* %CX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i8 %DH_0, i8* %DH
  store i16 -5632, i16* %DI
  store i8 0, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EAX_1, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 %ECX_0, i32* %ECX
  store i32 -94967296, i32* %EDI
  store i32 %EDX_0, i32* %EDX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 %EIP_27, i32* %EIP
  store i32 -1430928461, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_27, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 %RCX_0, i64* %RCX
  store i64 4200000000, i64* %RDI
  store i64 %RDX_0, i64* %RDX
  store i64 %RIP_28, i64* %RIP
  store i64 123123123123, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  store i16 -15437, i16* %SI
  store i8 -77, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  store i64 4200000000, i64* %R8
  %113 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %113, <4 x float>* %XMM0
  %114 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %114, <4 x float>* %XMM1
  %115 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %115, <8 x float>* %YMM0
  %116 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %116, <8 x float>* %YMM1
  %117 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %117, <16 x float>* %ZMM0
  %118 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %118, <16 x float>* %ZMM1
  store i8 0, i8* %R8B
  store i32 -94967296, i32* %R8D
  store i16 -5632, i16* %R8W
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
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
  br label %bb_4003D0

exit_fn_4003D0:                                   ; preds = %bb_4003EB, %bb_4003F8
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

bb_4003D0:                                        ; preds = %entry_fn_4003D0
  %RIP_1 = add i64 4195280, 1
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
  %CC_A_0 = icmp ugt i64 6295592, 6295592
  %CC_AE_0 = icmp uge i64 6295592, 6295592
  %CC_B_0 = icmp ult i64 6295592, 6295592
  %CC_BE_0 = icmp ule i64 6295592, 6295592
  %CC_L_0 = icmp slt i64 6295592, 6295592
  %CC_LE_0 = icmp sle i64 6295592, 6295592
  %CC_G_0 = icmp sgt i64 6295592, 6295592
  %CC_GE_0 = icmp sge i64 6295592, 6295592
  %CC_E_0 = icmp eq i64 6295592, 6295592
  %CC_NE_0 = icmp ne i64 6295592, 6295592
  %11 = sub i64 6295592, 6295592
  %ZF_0 = icmp eq i64 %11, 0
  %SF_0 = icmp slt i64 %11, 0
  %12 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 6295592, i64 6295592)
  %OF_0 = extractvalue { i64, i1 } %12, 1
  %13 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 6295592, i64 6295592)
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
  store i8 40, i8* %AL
  store i16 4136, i16* %AX
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 6295592, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195320, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1016, i16* %IP
  store i64 6295592, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195320, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_4003F8, label %bb_4003E1

bb_4003E1:                                        ; preds = %bb_4003D0
  %RIP_8 = add i64 4195297, 5
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
  store i32 4195320, i32* %EIP
  store i16 1016, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195320, i64* %RIP
  br i1 %CC_E_09, label %bb_4003F8, label %bb_4003EB

bb_4003EB:                                        ; preds = %bb_4003E1
  %RIP_17 = add i64 4195307, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_16, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_16, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_4003D0

bb_4003F8:                                        ; preds = %bb_4003E1, %bb_4003D0
  %RIP_13 = add i64 4195320, 1
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
  br label %exit_fn_4003D0
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.usub.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind
declare i8* @llvm.dc.translate.at(i8*) #2

define void @fn_400400(%regset* noalias nocapture) {
entry_fn_400400:
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
  br label %bb_400400

exit_fn_400400:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400400:                                        ; preds = %entry_fn_400400
  %RIP_1 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_0, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_1, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_13 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_22 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_2, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_21, i32* %EIP
  store i32 %ESP_3, i32* %ESP
  store i16 %IP_21, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400400

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_18 = add i64 4195384, 1
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
  br label %exit_fn_400400
}

define void @fn_400440(%regset* noalias nocapture) {
entry_fn_400440:
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
  br label %bb_400440

exit_fn_400440:                                   ; preds = %bb_400460, %bb_400449
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

bb_400440:                                        ; preds = %entry_fn_400440
  %RIP_1 = add i64 4195392, 7
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %6 = add i64 %RIP_1, 2100193
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
  store i32 4195424, i32* %EIP
  store i16 1120, i16* %IP
  store i64 4195424, i64* %RIP
  br i1 %CC_NE_0, label %bb_400460, label %bb_400449

bb_400449:                                        ; preds = %bb_400440
  %RIP_5 = add i64 4195401, 1
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
  store i64 4195410, i64* %33
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
  call void @fn_4003D0(%regset* %0)
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
  %44 = add i64 %RIP_9, 2100175
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
  br label %exit_fn_400440

bb_400460:                                        ; preds = %bb_400440
  %RIP_14 = add i64 4195424, 1
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
  br label %exit_fn_400440
}

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.ssub.with.overflow.i8(i8, i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i8, i1 } @llvm.usub.with.overflow.i8(i8, i8) #1

define void @fn_400470(%regset* noalias nocapture) {
entry_fn_400470:
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
  br label %bb_400470

exit_fn_400470:                                   ; preds = %bb_40042D, %bb_400438
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

bb_400470:                                        ; preds = %entry_fn_400470
  %RIP_1 = add i64 4195440, 1
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
  store i32 4195328, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i16 1024, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195328, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  store i16 %SP_1, i16* %SP
  store i8 %SPL_1, i8* %SPL
  br label %bb_400400

bb_400400:                                        ; preds = %bb_400470
  %RIP_7 = add i64 4195328, 5
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
  %RSI_2 = sub i64 6295592, 6295592
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
  store i32 4195384, i32* %EIP
  store i32 %ESI_4, i32* %ESI
  store i32 %ESP_2, i32* %ESP
  store i16 1080, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 %RSP_4, i64* %RBP
  store i64 4195384, i64* %RIP
  store i64 %RSI_5, i64* %RSI
  store i64 %RSP_4, i64* %RSP
  store i16 %SI_4, i16* %SI
  store i8 %SIL_4, i8* %SIL
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br i1 %ZF_1, label %bb_400438, label %bb_400423

bb_400423:                                        ; preds = %bb_400400
  %RIP_19 = add i64 4195363, 5
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
  store i32 4195384, i32* %EIP
  store i16 1080, i16* %IP
  store i64 0, i64* %RAX
  store i64 4195384, i64* %RIP
  br i1 %CC_E_0, label %bb_400438, label %bb_40042D

bb_40042D:                                        ; preds = %bb_400423
  %RIP_28 = add i64 4195373, 1
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
  store i16 4136, i16* %DI
  store i8 40, i8* %DIL
  store i32 %EBP_4, i32* %EBP
  store i32 6295592, i32* %EDI
  store i32 %EIP_26, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i16 %IP_26, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 6295592, i64* %RDI
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
  br label %exit_fn_400470

bb_400438:                                        ; preds = %bb_400423, %bb_400400
  %RIP_24 = add i64 4195384, 1
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
  br label %exit_fn_400470
}

define void @fn_400580(%regset* noalias nocapture) {
entry_fn_400580:
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
  br label %bb_400580

exit_fn_400580:                                   ; preds = %bb_400580
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_400580:                                        ; preds = %entry_fn_400580
  %RIP_1 = add i64 4195712, 1
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
  br label %exit_fn_400580
}

define void @fn_400584(%regset* noalias nocapture) {
entry_fn_400584:
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
  br label %bb_400584

exit_fn_400584:                                   ; preds = %bb_400584
  %1 = load i32, i32* %CtlSysEFLAGS
  store i32 %1, i32* %CtlSysEFLAGS_ptr
  %2 = load i32, i32* %EFLAGS
  store i32 %2, i32* %EFLAGS_ptr
  %3 = load i64, i64* %RIP
  store i64 %3, i64* %RIP_ptr
  %4 = load i64, i64* %RSP
  store i64 %4, i64* %RSP_ptr
  ret void

bb_400584:                                        ; preds = %entry_fn_400584
  %RIP_1 = add i64 4195716, 4
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
  br label %exit_fn_400584
}

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

; Function Attrs: nounwind readnone speculatable
declare { i64, i1 } @llvm.uadd.with.overflow.i64(i64, i64) #1

define void @fn_400510(%regset* noalias nocapture) {
entry_fn_400510:
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
  br label %bb_400510

exit_fn_400510:                                   ; preds = %bb_400566
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

bb_400510:                                        ; preds = %entry_fn_400510
  %RIP_1 = add i64 4195600, 2
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
  %R12_1 = add i64 %RIP_6, 2099502
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
  %RBP_1 = add i64 %RIP_8, 2099502
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
  store i64 4195649, i64* %29
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
  call void @fn_400370(%regset* %0)
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
  store i32 4195686, i32* %EIP
  store i16 1382, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195686, i64* %RIP
  br i1 %CC_E_0, label %bb_400566, label %bb_400546

bb_400546:                                        ; preds = %bb_400510
  %RIP_21 = add i64 4195654, 2
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
  br label %bb_400550

bb_400550:                                        ; preds = %bb_400550, %bb_400546
  %RIP_34 = add i64 4195664, 3
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
  store i64 4195677, i64* %126
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
  store i32 4195664, i32* %EIP
  store i16 1360, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 %RBX_6, i64* %RBX
  store i64 4195664, i64* %RIP
  br i1 %CC_NE_016, label %bb_400550, label %bb_400566

bb_400566:                                        ; preds = %bb_400550, %bb_400510
  %RIP_24 = add i64 4195686, 4
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
  br label %exit_fn_400510
}

define void @fn_400370(%regset* noalias nocapture) {
entry_fn_400370:
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
  br label %bb_400370

exit_fn_400370:                                   ; preds = %bb_400382
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

bb_400370:                                        ; preds = %entry_fn_400370
  %RIP_1 = add i64 4195184, 4
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
  %7 = add i64 %RIP_2, 2100349
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
  store i32 4195202, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 898, i16* %IP
  store i64 %RAX_0, i64* %RAX
  store i64 4195202, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  br i1 %CC_E_0, label %bb_400382, label %bb_400380

bb_400380:                                        ; preds = %bb_400370
  %RIP_7 = add i64 4195200, 2
  %EIP_5 = trunc i64 %RIP_7 to i32
  %IP_5 = trunc i64 %RIP_7 to i16
  %RAX_1 = load i64, i64* %RAX
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = sub i64 %RSP_2, 8
  %34 = inttoptr i64 %RSP_3 to i64*
  store i64 4195202, i64* %34
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
  br label %bb_400382

bb_400382:                                        ; preds = %bb_400380, %bb_400370
  %RIP_9 = add i64 4195202, 4
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
  br label %exit_fn_400370
}

define void @fn_4003C0(%regset* noalias nocapture) {
entry_fn_4003C0:
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
  br label %bb_4003C0

exit_fn_4003C0:                                   ; preds = %bb_4003C0
  %1 = load i64, i64* %RIP
  store i64 %1, i64* %RIP_ptr
  %2 = load i64, i64* %RSP
  store i64 %2, i64* %RSP_ptr
  ret void

bb_4003C0:                                        ; preds = %entry_fn_4003C0
  %RIP_1 = add i64 4195264, 1
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
  br label %exit_fn_4003C0
}

define void @fn_400390(%regset* noalias nocapture) {
entry_fn_400390:
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
  br label %bb_400390

exit_fn_400390:                                   ; preds = %bb_400390
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

bb_400390:                                        ; preds = %entry_fn_400390
  %RIP_1 = add i64 4195216, 2
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
  %22 = add i64 %RIP_11, 2100278
  %23 = inttoptr i64 %22 to i64*
  %24 = load i64, i64* %23, align 1
  %RSP_5 = sub i64 %RSP_4, 8
  %25 = inttoptr i64 %RSP_5 to i64*
  store i64 4195258, i64* %25
  %ESP_4 = trunc i64 %RSP_5 to i32
  %SP_4 = trunc i64 %RSP_5 to i16
  %SPL_4 = trunc i64 %RSP_5 to i8
  %26 = inttoptr i64 %24 to i8*
  %27 = call i8* @llvm.dc.translate.at(i8* %26)
  %28 = bitcast i8* %27 to void (%regset*)*
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i8 5, i8* %CH
  store i8 16, i8* %CL
  store i16 1296, i16* %CX
  store i8 %DH_0, i8* %DH
  store i16 1152, i16* %DI
  store i8 -128, i8* %DIL
  store i8 %DL_0, i8* %DL
  store i16 %DX_0, i16* %DX
  store i32 %EBP_1, i32* %EBP
  store i32 4195600, i32* %ECX
  store i32 4195456, i32* %EDI
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
  store i64 4195600, i64* %RCX
  store i64 4195456, i64* %RDI
  store i64 %RSP_1, i64* %RDX
  store i64 %RIP_11, i64* %RIP
  store i64 %RSI_0, i64* %RSI
  store i64 %RSP_5, i64* %RSP
  store i16 %SI_0, i16* %SI
  store i8 %SIL_0, i8* %SIL
  store i16 %SP_4, i16* %SP
  store i8 %SPL_4, i8* %SPL
  store i64 4195712, i64* %R8
  store i64 %RDX_0, i64* %R9
  store i8 -128, i8* %R8B
  store i8 %R9B_0, i8* %R9B
  store i32 4195712, i32* %R8D
  store i32 %R9D_0, i32* %R9D
  store i16 1408, i16* %R8W
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
  br label %exit_fn_400390
}

attributes #0 = { noreturn nounwind }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind }
