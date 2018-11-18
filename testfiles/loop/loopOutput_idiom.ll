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
  %ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
  %ZMM1_init = load <16 x float>, <16 x float>* %ZMM1_ptr
  %ZMM1 = alloca <16 x float>
  store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
  %5 = bitcast <16 x float> %ZMM1_init to i512
  %6 = trunc i512 %5 to i128
  %XMM1_init = bitcast i128 %6 to <4 x float>
  %XMM1 = alloca <4 x float>
  store <4 x float> %XMM1_init, <4 x float>* %XMM1
  %7 = bitcast <16 x float> %ZMM1_init to i512
  %8 = trunc i512 %7 to i256
  %YMM1_init = bitcast i256 %8 to <8 x float>
  %YMM1 = alloca <8 x float>
  store <8 x float> %YMM1_init, <8 x float>* %YMM1
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
  %9 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %9 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
  %EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
  %EFLAGS_init = load i32, i32* %EFLAGS_ptr
  %EFLAGS = alloca i32
  store i32 %EFLAGS_init, i32* %EFLAGS
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_400503
  %10 = load i32, i32* %CtlSysEFLAGS
  store i32 %10, i32* %CtlSysEFLAGS_ptr
  %11 = load i32, i32* %EFLAGS
  store i32 %11, i32* %EFLAGS_ptr
  %12 = load i64, i64* %RAX
  store i64 %12, i64* %RAX_ptr
  %13 = load i64, i64* %RBP
  store i64 %13, i64* %RBP_ptr
  %14 = load i64, i64* %RIP
  store i64 %14, i64* %RIP_ptr
  %15 = load i64, i64* %RSP
  store i64 %15, i64* %RSP_ptr
  %16 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %16, <16 x float>* %ZMM0_ptr
  %17 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %17, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %18 = sub i64 %RSP_0, 8
  %19 = inttoptr i64 %18 to i64*
  store i64 %RBP_0, i64* %19, align 1
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
  %RIP_3 = add i64 %RIP_2, 8
  %EIP_2 = trunc i64 %RIP_3 to i32
  %IP_2 = trunc i64 %RIP_3 to i16
  %20 = add i64 %RIP_3, 268
  %21 = inttoptr i64 %20 to double*
  %22 = load double, double* %21, align 1
  %23 = bitcast double %22 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %24 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %24 to i128
  %25 = zext i64 %23 to i128
  %26 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %25, %26
  %27 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %27 to i256
  %28 = zext i128 %XMM0_1 to i256
  %29 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %28, %29
  %30 = bitcast <16 x float> %ZMM0_0 to i512
  %31 = zext i128 %XMM0_1 to i512
  %32 = and i512 %30, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %31, %32
  %RIP_4 = add i64 %RIP_3, 8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %33 = add i64 %RIP_4, 268
  %34 = inttoptr i64 %33 to double*
  %35 = load double, double* %34, align 1
  %36 = bitcast double %35 to i64
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %37 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %37 to i128
  %38 = zext i64 %36 to i128
  %39 = and i128 %XMM1_0, -18446744073709551616
  %XMM1_1 = or i128 %38, %39
  %40 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %40 to i256
  %41 = zext i128 %XMM1_1 to i256
  %42 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %41, %42
  %43 = bitcast <16 x float> %ZMM1_0 to i512
  %44 = zext i128 %XMM1_1 to i512
  %45 = and i512 %43, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %44, %45
  %RIP_5 = add i64 %RIP_4, 7
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %46 = add i64 %RSP_1, -4
  %47 = inttoptr i64 %46 to i32*
  store i32 0, i32* %47, align 1
  %RIP_6 = add i64 %RIP_5, 7
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %48 = add i64 %RSP_1, -8
  %49 = inttoptr i64 %48 to i32*
  store i32 5, i32* %49, align 1
  %RIP_7 = add i64 %RIP_6, 7
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %50 = add i64 %RSP_1, -12
  %51 = inttoptr i64 %50 to i32*
  store i32 625, i32* %51, align 1
  %RIP_8 = add i64 %RIP_7, 5
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %52 = trunc i128 %XMM1_1 to i64
  %53 = bitcast i64 %52 to double
  %54 = add i64 %RSP_1, -24
  %55 = inttoptr i64 %54 to double*
  store double %53, double* %55, align 1
  %RIP_9 = add i64 %RIP_8, 5
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %56 = trunc i128 %XMM0_1 to i64
  %57 = bitcast i64 %56 to double
  %58 = add i64 %RSP_1, -32
  %59 = inttoptr i64 %58 to double*
  store double %57, double* %59, align 1
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %EBP_0, i32* %EBP
  store i32 %EIP_8, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 %IP_8, i16* %IP
  store i64 %RSP_1, i64* %RBP
  store i64 %RIP_9, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  %60 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %60, <4 x float>* %XMM0
  %61 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %61, <4 x float>* %XMM1
  %62 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %62, <8 x float>* %YMM0
  %63 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %63, <8 x float>* %YMM1
  %64 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %64, <16 x float>* %ZMM0
  %65 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %65, <16 x float>* %ZMM1
  br label %bb_4004B3

bb_4004B3:                                        ; preds = %bb_4004BF, %bb_400480
  %RIP_19 = add i64 4195507, 3
  %EIP_15 = trunc i64 %RIP_19 to i32
  %IP_15 = trunc i64 %RIP_19 to i16
  %RBP_2 = load i64, i64* %RBP
  %66 = add i64 %RBP_2, -8
  %67 = inttoptr i64 %66 to i32*
  %EAX_1 = load i32, i32* %67, align 1
  %RAX_2 = load i64, i64* %RAX
  %RAX_3 = zext i32 %EAX_1 to i64
  %AX_1 = trunc i32 %EAX_1 to i16
  %AL_1 = trunc i32 %EAX_1 to i8
  %68 = lshr i32 %EAX_1, 8
  %AH_1 = trunc i32 %68 to i8
  %RIP_20 = add i64 %RIP_19, 3
  %EIP_16 = trunc i64 %RIP_20 to i32
  %IP_16 = trunc i64 %RIP_20 to i16
  %69 = add i64 %RBP_2, -12
  %70 = inttoptr i64 %69 to i32*
  %71 = load i32, i32* %70, align 1
  %CC_A_0 = icmp ugt i32 %EAX_1, %71
  %CC_AE_0 = icmp uge i32 %EAX_1, %71
  %CC_B_0 = icmp ult i32 %EAX_1, %71
  %CC_BE_0 = icmp ule i32 %EAX_1, %71
  %CC_L_0 = icmp slt i32 %EAX_1, %71
  %CC_LE_0 = icmp sle i32 %EAX_1, %71
  %CC_G_0 = icmp sgt i32 %EAX_1, %71
  %CC_GE_0 = icmp sge i32 %EAX_1, %71
  %CC_E_0 = icmp eq i32 %EAX_1, %71
  %CC_NE_0 = icmp ne i32 %EAX_1, %71
  %72 = sub i32 %EAX_1, %71
  %ZF_01 = icmp eq i32 %72, 0
  %SF_02 = icmp slt i32 %72, 0
  %73 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_1, i32 %71)
  %OF_0 = extractvalue { i32, i1 } %73, 1
  %74 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_1, i32 %71)
  %CF_0 = extractvalue { i32, i1 } %74, 1
  %75 = trunc i32 %72 to i8
  %76 = call i8 @llvm.ctpop.i8(i8 %75)
  %77 = trunc i8 %76 to i1
  %PF_03 = icmp eq i1 %77, false
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %78 = zext i1 %CF_0 to i32
  %79 = shl i32 %78, 0
  %80 = or i32 %79, %CtlSysEFLAGS_1
  %81 = zext i1 %PF_03 to i32
  %82 = shl i32 %81, 2
  %83 = or i32 %82, %80
  %84 = zext i1 false to i32
  %85 = shl i32 %84, 4
  %86 = or i32 %85, %83
  %87 = zext i1 %ZF_01 to i32
  %88 = shl i32 %87, 6
  %89 = or i32 %88, %86
  %90 = zext i1 %SF_02 to i32
  %91 = shl i32 %90, 7
  %92 = or i32 %91, %89
  %93 = zext i1 %OF_0 to i32
  %94 = shl i32 %93, 11
  %EFLAGS_2 = or i32 %94, %92
  %RIP_21 = add i64 %RIP_20, 6
  %EIP_17 = trunc i64 %RIP_21 to i32
  %IP_17 = trunc i64 %RIP_21 to i16
  store i8 %AH_1, i8* %AH
  store i8 %AL_1, i8* %AL
  store i16 %AX_1, i16* %AX
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 %EAX_1, i32* %EAX
  store i32 %EFLAGS_2, i32* %EFLAGS
  store i32 4195531, i32* %EIP
  store i16 1227, i16* %IP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_2, i64* %RBP
  store i64 4195531, i64* %RIP
  br i1 %CC_GE_0, label %bb_4004CB, label %bb_4004BF

bb_4004BF:                                        ; preds = %bb_4004B3
  %RIP_11 = add i64 4195519, 4
  %EIP_9 = trunc i64 %RIP_11 to i32
  %IP_9 = trunc i64 %RIP_11 to i16
  %RBP_1 = load i64, i64* %RBP
  %95 = add i64 %RBP_1, -8
  %96 = inttoptr i64 %95 to i32*
  %97 = load i32, i32* %96, align 1
  %EAX_0 = mul i32 %97, 5
  %RAX_0 = load i64, i64* %RAX
  %RAX_1 = zext i32 %EAX_0 to i64
  %AX_0 = trunc i32 %EAX_0 to i16
  %AL_0 = trunc i32 %EAX_0 to i8
  %98 = lshr i32 %EAX_0, 8
  %AH_0 = trunc i32 %98 to i8
  %EFLAGS_0 = load i32, i32* %EFLAGS
  %RIP_12 = add i64 %RIP_11, 3
  %EIP_10 = trunc i64 %RIP_12 to i32
  %IP_10 = trunc i64 %RIP_12 to i16
  %99 = add i64 %RBP_1, -8
  %100 = inttoptr i64 %99 to i32*
  store i32 %EAX_0, i32* %100, align 1
  %RIP_13 = add i64 %RIP_12, 5
  %EIP_11 = trunc i64 %RIP_13 to i32
  %IP_11 = trunc i64 %RIP_13 to i16
  %ZF_0 = icmp eq i32 %EAX_0, 0
  %SF_0 = icmp slt i32 %EAX_0, 0
  %101 = trunc i32 %EAX_0 to i8
  %102 = call i8 @llvm.ctpop.i8(i8 %101)
  %103 = trunc i8 %102 to i1
  %PF_0 = icmp eq i1 %103, false
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %104 = zext i1 false to i32
  %105 = shl i32 %104, 0
  %106 = or i32 %105, %CtlSysEFLAGS_0
  %107 = zext i1 %PF_0 to i32
  %108 = shl i32 %107, 2
  %109 = or i32 %108, %106
  %110 = zext i1 false to i32
  %111 = shl i32 %110, 4
  %112 = or i32 %111, %109
  %113 = zext i1 %ZF_0 to i32
  %114 = shl i32 %113, 6
  %115 = or i32 %114, %112
  %116 = zext i1 %SF_0 to i32
  %117 = shl i32 %116, 7
  %118 = or i32 %117, %115
  %119 = zext i1 false to i32
  %120 = shl i32 %119, 11
  %EFLAGS_1 = or i32 %120, %118
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EAX_0, i32* %EAX
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 4195507, i32* %EIP
  store i16 1203, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 4195507, i64* %RIP
  br label %bb_4004B3

bb_4004CB:                                        ; preds = %bb_4004B3
  %RIP_16 = add i64 4195531, 5
  %EIP_13 = trunc i64 %RIP_16 to i32
  %IP_13 = trunc i64 %RIP_16 to i16
  store i32 4195536, i32* %EIP
  store i16 1232, i16* %IP
  store i64 4195536, i64* %RIP
  br label %bb_4004D0

bb_4004D0:                                        ; preds = %bb_4004E0, %bb_4004CB
  %RIP_24 = add i64 4195536, 5
  %EIP_19 = trunc i64 %RIP_24 to i32
  %IP_19 = trunc i64 %RIP_24 to i16
  %RBP_3 = load i64, i64* %RBP
  %121 = add i64 %RBP_3, -24
  %122 = inttoptr i64 %121 to double*
  %123 = load double, double* %122, align 1
  %124 = bitcast double %123 to i64
  %ZMM0_2 = load <16 x float>, <16 x float>* %ZMM0
  %125 = bitcast <16 x float> %ZMM0_2 to i512
  %XMM0_2 = trunc i512 %125 to i128
  %126 = zext i64 %124 to i128
  %127 = and i128 %XMM0_2, -18446744073709551616
  %XMM0_3 = or i128 %126, %127
  %128 = bitcast <16 x float> %ZMM0_2 to i512
  %YMM0_2 = trunc i512 %128 to i256
  %129 = zext i128 %XMM0_3 to i256
  %130 = and i256 %YMM0_2, -340282366920938463463374607431768211456
  %YMM0_3 = or i256 %129, %130
  %131 = bitcast <16 x float> %ZMM0_2 to i512
  %132 = zext i128 %XMM0_3 to i512
  %133 = and i512 %131, -340282366920938463463374607431768211456
  %ZMM0_3 = or i512 %132, %133
  %RIP_25 = add i64 %RIP_24, 5
  %EIP_20 = trunc i64 %RIP_25 to i32
  %IP_20 = trunc i64 %RIP_25 to i16
  %134 = trunc i128 %XMM0_3 to i64
  %135 = bitcast i64 %134 to double
  %136 = add i64 %RBP_3, -32
  %137 = inttoptr i64 %136 to double*
  %138 = load double, double* %137, align 1
  %ZF_04 = fcmp ueq double %135, %138
  %PF_05 = fcmp uno double %135, %138
  %CF_06 = fcmp ult double %135, %138
  %CtlSysEFLAGS_2 = load i32, i32* %CtlSysEFLAGS
  %139 = zext i1 %CF_06 to i32
  %140 = shl i32 %139, 0
  %141 = or i32 %140, %CtlSysEFLAGS_2
  %142 = zext i1 %PF_05 to i32
  %143 = shl i32 %142, 2
  %144 = or i32 %143, %141
  %145 = zext i1 false to i32
  %146 = shl i32 %145, 4
  %147 = or i32 %146, %144
  %148 = zext i1 %ZF_04 to i32
  %149 = shl i32 %148, 6
  %150 = or i32 %149, %147
  %151 = zext i1 false to i32
  %152 = shl i32 %151, 7
  %153 = or i32 %152, %150
  %154 = zext i1 false to i32
  %155 = shl i32 %154, 11
  %EFLAGS_3 = or i32 %155, %153
  %RIP_26 = add i64 %RIP_25, 6
  %EIP_21 = trunc i64 %RIP_26 to i32
  %IP_21 = trunc i64 %RIP_26 to i16
  store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
  store i32 %EFLAGS_3, i32* %EFLAGS
  store i32 4195587, i32* %EIP
  store i16 1283, i16* %IP
  store i64 %RBP_3, i64* %RBP
  store i64 4195587, i64* %RIP
  %156 = bitcast i128 %XMM0_3 to <4 x float>
  store <4 x float> %156, <4 x float>* %XMM0
  %157 = bitcast i256 %YMM0_3 to <8 x float>
  store <8 x float> %157, <8 x float>* %YMM0
  %158 = bitcast i512 %ZMM0_3 to <16 x float>
  store <16 x float> %158, <16 x float>* %ZMM0
  br i1 %CF_06, label %bb_400503, label %bb_4004E0

bb_4004E0:                                        ; preds = %bb_4004D0
  %RIP_29 = add i64 4195552, 5
  %EIP_23 = trunc i64 %RIP_29 to i32
  %IP_23 = trunc i64 %RIP_29 to i16
  %RBP_4 = load i64, i64* %RBP
  %159 = add i64 %RBP_4, -24
  %160 = inttoptr i64 %159 to double*
  %161 = load double, double* %160, align 1
  %162 = bitcast double %161 to i64
  %ZMM0_4 = load <16 x float>, <16 x float>* %ZMM0
  %163 = bitcast <16 x float> %ZMM0_4 to i512
  %XMM0_4 = trunc i512 %163 to i128
  %164 = zext i64 %162 to i128
  %165 = and i128 %XMM0_4, -18446744073709551616
  %XMM0_5 = or i128 %164, %165
  %166 = bitcast <16 x float> %ZMM0_4 to i512
  %YMM0_4 = trunc i512 %166 to i256
  %167 = zext i128 %XMM0_5 to i256
  %168 = and i256 %YMM0_4, -340282366920938463463374607431768211456
  %YMM0_5 = or i256 %167, %168
  %169 = bitcast <16 x float> %ZMM0_4 to i512
  %170 = zext i128 %XMM0_5 to i512
  %171 = and i512 %169, -340282366920938463463374607431768211456
  %ZMM0_5 = or i512 %170, %171
  %RIP_30 = add i64 %RIP_29, 5
  %EIP_24 = trunc i64 %RIP_30 to i32
  %IP_24 = trunc i64 %RIP_30 to i16
  %172 = trunc i128 %XMM0_5 to i64
  %173 = bitcast i64 %172 to double
  %174 = add i64 %RBP_4, -32
  %175 = inttoptr i64 %174 to double*
  %176 = load double, double* %175, align 1
  %177 = fdiv double %173, %176
  %178 = bitcast double %177 to i64
  %179 = zext i64 %178 to i128
  %180 = and i128 %XMM0_5, -18446744073709551616
  %XMM0_6 = or i128 %179, %180
  %181 = zext i128 %XMM0_6 to i256
  %182 = and i256 %YMM0_5, -340282366920938463463374607431768211456
  %YMM0_6 = or i256 %181, %182
  %183 = zext i128 %XMM0_6 to i512
  %184 = and i512 %ZMM0_5, -340282366920938463463374607431768211456
  %ZMM0_6 = or i512 %183, %184
  %RIP_31 = add i64 %RIP_30, 5
  %EIP_25 = trunc i64 %RIP_31 to i32
  %IP_25 = trunc i64 %RIP_31 to i16
  %185 = trunc i128 %XMM0_6 to i64
  %186 = bitcast i64 %185 to double
  %187 = add i64 %RBP_4, -40
  %188 = inttoptr i64 %187 to double*
  store double %186, double* %188, align 1
  %RIP_32 = add i64 %RIP_31, 5
  %EIP_26 = trunc i64 %RIP_32 to i32
  %IP_26 = trunc i64 %RIP_32 to i16
  %189 = add i64 %RBP_4, -40
  %190 = inttoptr i64 %189 to double*
  %191 = load double, double* %190, align 1
  %192 = bitcast double %191 to i64
  %193 = zext i64 %192 to i128
  %194 = and i128 %XMM0_6, -18446744073709551616
  %XMM0_7 = or i128 %193, %194
  %195 = zext i128 %XMM0_7 to i256
  %196 = and i256 %YMM0_6, -340282366920938463463374607431768211456
  %YMM0_7 = or i256 %195, %196
  %197 = zext i128 %XMM0_7 to i512
  %198 = and i512 %ZMM0_6, -340282366920938463463374607431768211456
  %ZMM0_7 = or i512 %197, %198
  %RIP_33 = add i64 %RIP_32, 5
  %EIP_27 = trunc i64 %RIP_33 to i32
  %IP_27 = trunc i64 %RIP_33 to i16
  %199 = trunc i128 %XMM0_7 to i64
  %200 = bitcast i64 %199 to double
  %201 = add i64 %RBP_4, -32
  %202 = inttoptr i64 %201 to double*
  %203 = load double, double* %202, align 1
  %204 = fadd double %200, %203
  %205 = bitcast double %204 to i64
  %206 = zext i64 %205 to i128
  %207 = and i128 %XMM0_7, -18446744073709551616
  %XMM0_8 = or i128 %206, %207
  %208 = zext i128 %XMM0_8 to i256
  %209 = and i256 %YMM0_7, -340282366920938463463374607431768211456
  %YMM0_8 = or i256 %208, %209
  %210 = zext i128 %XMM0_8 to i512
  %211 = and i512 %ZMM0_7, -340282366920938463463374607431768211456
  %ZMM0_8 = or i512 %210, %211
  %RIP_34 = add i64 %RIP_33, 5
  %EIP_28 = trunc i64 %RIP_34 to i32
  %IP_28 = trunc i64 %RIP_34 to i16
  %212 = trunc i128 %XMM0_8 to i64
  %213 = bitcast i64 %212 to double
  %214 = add i64 %RBP_4, -32
  %215 = inttoptr i64 %214 to double*
  store double %213, double* %215, align 1
  %RIP_35 = add i64 %RIP_34, 5
  %EIP_29 = trunc i64 %RIP_35 to i32
  %IP_29 = trunc i64 %RIP_35 to i16
  store i32 4195536, i32* %EIP
  store i16 1232, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 4195536, i64* %RIP
  %216 = bitcast i128 %XMM0_8 to <4 x float>
  store <4 x float> %216, <4 x float>* %XMM0
  %217 = bitcast i256 %YMM0_8 to <8 x float>
  store <8 x float> %217, <8 x float>* %YMM0
  %218 = bitcast i512 %ZMM0_8 to <16 x float>
  store <16 x float> %218, <16 x float>* %ZMM0
  br label %bb_4004D0

bb_400503:                                        ; preds = %bb_4004D0
  %RIP_38 = add i64 4195587, 2
  %EIP_31 = trunc i64 %RIP_38 to i32
  %IP_31 = trunc i64 %RIP_38 to i16
  %RAX_4 = load i64, i64* %RAX
  %EAX_2 = trunc i64 %RAX_4 to i32
  %EAX_3 = xor i32 %EAX_2, %EAX_2
  %RAX_5 = zext i32 %EAX_3 to i64
  %AX_2 = trunc i32 %EAX_3 to i16
  %AL_2 = trunc i32 %EAX_3 to i8
  %219 = lshr i32 %EAX_3, 8
  %AH_2 = trunc i32 %219 to i8
  %EFLAGS_4 = load i32, i32* %EFLAGS
  %RIP_39 = add i64 %RIP_38, 1
  %EIP_32 = trunc i64 %RIP_39 to i32
  %IP_32 = trunc i64 %RIP_39 to i16
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = add i64 %RSP_2, 8
  %ESP_1 = trunc i64 %RSP_3 to i32
  %SP_1 = trunc i64 %RSP_3 to i16
  %SPL_1 = trunc i64 %RSP_3 to i8
  %220 = sub i64 %RSP_3, 8
  %221 = inttoptr i64 %220 to i64*
  %RBP_5 = load i64, i64* %221, align 1
  %EBP_1 = trunc i64 %RBP_5 to i32
  %BP_1 = trunc i64 %RBP_5 to i16
  %BPL_1 = trunc i64 %RBP_5 to i8
  %RIP_40 = add i64 %RIP_39, 1
  %EIP_33 = trunc i64 %RIP_40 to i32
  %IP_33 = trunc i64 %RIP_40 to i16
  %RSP_4 = add i64 %RSP_3, 8
  %222 = inttoptr i64 %RSP_3 to i64*
  %RIP_41 = load i64, i64* %222
  %ESP_2 = trunc i64 %RSP_4 to i32
  %SP_2 = trunc i64 %RSP_4 to i16
  %SPL_2 = trunc i64 %RSP_4 to i8
  %EIP_34 = trunc i64 %RIP_41 to i32
  %IP_34 = trunc i64 %RIP_41 to i16
  %ZF_07 = icmp eq i32 %EAX_3, 0
  %SF_08 = icmp slt i32 %EAX_3, 0
  %223 = trunc i32 %EAX_3 to i8
  %224 = call i8 @llvm.ctpop.i8(i8 %223)
  %225 = trunc i8 %224 to i1
  %PF_09 = icmp eq i1 %225, false
  %CtlSysEFLAGS_3 = load i32, i32* %CtlSysEFLAGS
  %226 = zext i1 false to i32
  %227 = shl i32 %226, 0
  %228 = or i32 %227, %CtlSysEFLAGS_3
  %229 = zext i1 %PF_09 to i32
  %230 = shl i32 %229, 2
  %231 = or i32 %230, %228
  %232 = zext i1 false to i32
  %233 = shl i32 %232, 4
  %234 = or i32 %233, %231
  %235 = zext i1 %ZF_07 to i32
  %236 = shl i32 %235, 6
  %237 = or i32 %236, %234
  %238 = zext i1 %SF_08 to i32
  %239 = shl i32 %238, 7
  %240 = or i32 %239, %237
  %241 = zext i1 false to i32
  %242 = shl i32 %241, 11
  %EFLAGS_5 = or i32 %242, %240
  store i8 %AH_2, i8* %AH
  store i8 %AL_2, i8* %AL
  store i16 %AX_2, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
  store i32 %EAX_3, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 %EFLAGS_5, i32* %EFLAGS
  store i32 %EIP_34, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_34, i16* %IP
  store i64 %RAX_5, i64* %RAX
  store i64 %RBP_5, i64* %RBP
  store i64 %RIP_41, i64* %RIP
  store i64 %RSP_4, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare i8 @llvm.ctpop.i8(i8) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.usub.with.overflow.i32(i32, i32) #1

define i32 @main(i32, i8**) {
  %3 = alloca %regset, align 64
  %4 = alloca [1024 x i8], align 64
  %5 = getelementptr inbounds [1024 x i8], [1024 x i8]* %4, i32 0, i32 0
  call void @main_init_regset(%regset* %3, i8* %5, i32 1024, i32 %0, i8** %1)
  call void @fn_400480(%regset* %3)
  %6 = call i32 @main_fini_regset(%regset* %3)
  ret i32 %6
}
