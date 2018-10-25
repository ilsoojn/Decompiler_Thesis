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
