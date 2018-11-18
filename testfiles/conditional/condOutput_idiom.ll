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
  %CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
  %CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
  %CtlSysEFLAGS = alloca i32
  store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
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
  %9 = lshr i64 %RAX_init, 8
  %AH_init = trunc i64 %9 to i8
  %AH = alloca i8
  store i8 %AH_init, i8* %AH
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
  br label %bb_400480

exit_fn_400480:                                   ; preds = %bb_40053F
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
  %16 = load i64, i64* %RIP
  store i64 %16, i64* %RIP_ptr
  %17 = load i64, i64* %RSP
  store i64 %17, i64* %RSP_ptr
  %18 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %18, <16 x float>* %ZMM0_ptr
  %19 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %19, <16 x float>* %ZMM1_ptr
  ret void

bb_400480:                                        ; preds = %entry_fn_400480
  %RIP_1 = add i64 4195456, 1
  %EIP_0 = trunc i64 %RIP_1 to i32
  %IP_0 = trunc i64 %RIP_1 to i16
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %20 = sub i64 %RSP_0, 8
  %21 = inttoptr i64 %20 to i64*
  store i64 %RBP_0, i64* %21, align 1
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
  %22 = add i64 %RIP_3, 332
  %23 = inttoptr i64 %22 to double*
  %24 = load double, double* %23, align 1
  %25 = bitcast double %24 to i64
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %26 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %26 to i128
  %27 = zext i64 %25 to i128
  %28 = and i128 %XMM0_0, -18446744073709551616
  %XMM0_1 = or i128 %27, %28
  %29 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %29 to i256
  %30 = zext i128 %XMM0_1 to i256
  %31 = and i256 %YMM0_0, -340282366920938463463374607431768211456
  %YMM0_1 = or i256 %30, %31
  %32 = bitcast <16 x float> %ZMM0_0 to i512
  %33 = zext i128 %XMM0_1 to i512
  %34 = and i512 %32, -340282366920938463463374607431768211456
  %ZMM0_1 = or i512 %33, %34
  %RIP_4 = add i64 %RIP_3, 8
  %EIP_3 = trunc i64 %RIP_4 to i32
  %IP_3 = trunc i64 %RIP_4 to i16
  %35 = add i64 %RIP_4, 332
  %36 = inttoptr i64 %35 to double*
  %37 = load double, double* %36, align 1
  %38 = bitcast double %37 to i64
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %39 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %39 to i128
  %40 = zext i64 %38 to i128
  %41 = and i128 %XMM1_0, -18446744073709551616
  %XMM1_1 = or i128 %40, %41
  %42 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %42 to i256
  %43 = zext i128 %XMM1_1 to i256
  %44 = and i256 %YMM1_0, -340282366920938463463374607431768211456
  %YMM1_1 = or i256 %43, %44
  %45 = bitcast <16 x float> %ZMM1_0 to i512
  %46 = zext i128 %XMM1_1 to i512
  %47 = and i512 %45, -340282366920938463463374607431768211456
  %ZMM1_1 = or i512 %46, %47
  %RIP_5 = add i64 %RIP_4, 7
  %EIP_4 = trunc i64 %RIP_5 to i32
  %IP_4 = trunc i64 %RIP_5 to i16
  %48 = add i64 %RSP_1, -4
  %49 = inttoptr i64 %48 to i32*
  store i32 0, i32* %49, align 1
  %RIP_6 = add i64 %RIP_5, 5
  %EIP_5 = trunc i64 %RIP_6 to i32
  %IP_5 = trunc i64 %RIP_6 to i16
  %50 = trunc i128 %XMM1_1 to i64
  %51 = bitcast i64 %50 to double
  %52 = add i64 %RSP_1, -16
  %53 = inttoptr i64 %52 to double*
  store double %51, double* %53, align 1
  %RIP_7 = add i64 %RIP_6, 5
  %EIP_6 = trunc i64 %RIP_7 to i32
  %IP_6 = trunc i64 %RIP_7 to i16
  %54 = trunc i128 %XMM0_1 to i64
  %55 = bitcast i64 %54 to double
  %56 = add i64 %RSP_1, -24
  %57 = inttoptr i64 %56 to double*
  store double %55, double* %57, align 1
  %RIP_8 = add i64 %RIP_7, 5
  %EIP_7 = trunc i64 %RIP_8 to i32
  %IP_7 = trunc i64 %RIP_8 to i16
  %58 = add i64 %RSP_1, -16
  %59 = inttoptr i64 %58 to double*
  %60 = load double, double* %59, align 1
  %61 = bitcast double %60 to i64
  %62 = zext i64 %61 to i128
  %63 = and i128 %XMM0_1, -18446744073709551616
  %XMM0_2 = or i128 %62, %63
  %64 = zext i128 %XMM0_2 to i256
  %65 = and i256 %YMM0_1, -340282366920938463463374607431768211456
  %YMM0_2 = or i256 %64, %65
  %66 = zext i128 %XMM0_2 to i512
  %67 = and i512 %ZMM0_1, -340282366920938463463374607431768211456
  %ZMM0_2 = or i512 %66, %67
  %RIP_9 = add i64 %RIP_8, 5
  %EIP_8 = trunc i64 %RIP_9 to i32
  %IP_8 = trunc i64 %RIP_9 to i16
  %68 = trunc i128 %XMM0_2 to i64
  %69 = bitcast i64 %68 to double
  %70 = add i64 %RSP_1, -24
  %71 = inttoptr i64 %70 to double*
  %72 = load double, double* %71, align 1
  %ZF_0 = fcmp ueq double %69, %72
  %PF_0 = fcmp uno double %69, %72
  %CF_0 = fcmp ult double %69, %72
  %CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
  %73 = zext i1 %CF_0 to i32
  %74 = shl i32 %73, 0
  %75 = or i32 %74, %CtlSysEFLAGS_0
  %76 = zext i1 %PF_0 to i32
  %77 = shl i32 %76, 2
  %78 = or i32 %77, %75
  %79 = zext i1 false to i32
  %80 = shl i32 %79, 4
  %81 = or i32 %80, %78
  %82 = zext i1 %ZF_0 to i32
  %83 = shl i32 %82, 6
  %84 = or i32 %83, %81
  %85 = zext i1 false to i32
  %86 = shl i32 %85, 7
  %87 = or i32 %86, %84
  %88 = zext i1 false to i32
  %89 = shl i32 %88, 11
  %EFLAGS_0 = or i32 %89, %87
  %RIP_10 = add i64 %RIP_9, 6
  %EIP_9 = trunc i64 %RIP_10 to i32
  %IP_9 = trunc i64 %RIP_10 to i16
  %CC_NE_0 = xor i1 %ZF_0, true
  store i16 %BP_0, i16* %BP
  store i8 %BPL_0, i8* %BPL
  store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
  store i32 %EBP_0, i32* %EBP
  store i32 %EFLAGS_0, i32* %EFLAGS
  store i32 4195527, i32* %EIP
  store i32 %ESP_0, i32* %ESP
  store i16 1223, i16* %IP
  store i64 %RSP_1, i64* %RBP
  store i64 4195527, i64* %RIP
  store i64 %RSP_1, i64* %RSP
  store i16 %SP_0, i16* %SP
  store i8 %SPL_0, i8* %SPL
  %90 = bitcast i128 %XMM0_2 to <4 x float>
  store <4 x float> %90, <4 x float>* %XMM0
  %91 = bitcast i128 %XMM1_1 to <4 x float>
  store <4 x float> %91, <4 x float>* %XMM1
  %92 = bitcast i256 %YMM0_2 to <8 x float>
  store <8 x float> %92, <8 x float>* %YMM0
  %93 = bitcast i256 %YMM1_1 to <8 x float>
  store <8 x float> %93, <8 x float>* %YMM1
  %94 = bitcast i512 %ZMM0_2 to <16 x float>
  store <16 x float> %94, <16 x float>* %ZMM0
  %95 = bitcast i512 %ZMM1_1 to <16 x float>
  store <16 x float> %95, <16 x float>* %ZMM1
  br i1 %CC_NE_0, label %bb_4004C7, label %bb_4004B5

bb_4004B5:                                        ; preds = %bb_400480
  %RIP_13 = add i64 4195509, 6
  %EIP_11 = trunc i64 %RIP_13 to i32
  %IP_11 = trunc i64 %RIP_13 to i16
  %EFLAGS_1 = load i32, i32* %EFLAGS
  %96 = lshr i32 %EFLAGS_1, 2
  %PF_01 = trunc i32 %96 to i1
  store i32 %EFLAGS_1, i32* %EFLAGS
  store i32 4195527, i32* %EIP
  store i16 1223, i16* %IP
  store i64 4195527, i64* %RIP
  br i1 %PF_01, label %bb_4004C7, label %bb_4004BB

bb_4004BB:                                        ; preds = %bb_4004B5
  %RIP_22 = add i64 4195515, 7
  %EIP_18 = trunc i64 %RIP_22 to i32
  %IP_18 = trunc i64 %RIP_22 to i16
  %RBP_2 = load i64, i64* %RBP
  %97 = add i64 %RBP_2, -28
  %98 = inttoptr i64 %97 to i32*
  store i32 0, i32* %98, align 1
  %RIP_23 = add i64 %RIP_22, 5
  %EIP_19 = trunc i64 %RIP_23 to i32
  %IP_19 = trunc i64 %RIP_23 to i16
  store i32 4195571, i32* %EIP
  store i16 1267, i16* %IP
  store i64 %RBP_2, i64* %RBP
  store i64 4195571, i64* %RIP
  br label %bb_4004F3

bb_4004C7:                                        ; preds = %bb_4004B5, %bb_400480
  %RIP_16 = add i64 4195527, 5
  %EIP_13 = trunc i64 %RIP_16 to i32
  %IP_13 = trunc i64 %RIP_16 to i16
  %RBP_1 = load i64, i64* %RBP
  %99 = add i64 %RBP_1, -16
  %100 = inttoptr i64 %99 to double*
  %101 = load double, double* %100, align 1
  %102 = bitcast double %101 to i64
  %ZMM0_3 = load <16 x float>, <16 x float>* %ZMM0
  %103 = bitcast <16 x float> %ZMM0_3 to i512
  %XMM0_3 = trunc i512 %103 to i128
  %104 = zext i64 %102 to i128
  %105 = and i128 %XMM0_3, -18446744073709551616
  %XMM0_4 = or i128 %104, %105
  %106 = bitcast <16 x float> %ZMM0_3 to i512
  %YMM0_3 = trunc i512 %106 to i256
  %107 = zext i128 %XMM0_4 to i256
  %108 = and i256 %YMM0_3, -340282366920938463463374607431768211456
  %YMM0_4 = or i256 %107, %108
  %109 = bitcast <16 x float> %ZMM0_3 to i512
  %110 = zext i128 %XMM0_4 to i512
  %111 = and i512 %109, -340282366920938463463374607431768211456
  %ZMM0_4 = or i512 %110, %111
  %RIP_17 = add i64 %RIP_16, 5
  %EIP_14 = trunc i64 %RIP_17 to i32
  %IP_14 = trunc i64 %RIP_17 to i16
  %112 = add i64 %RBP_1, -24
  %113 = inttoptr i64 %112 to double*
  %114 = load double, double* %113, align 1
  %115 = bitcast double %114 to i64
  %ZMM1_2 = load <16 x float>, <16 x float>* %ZMM1
  %116 = bitcast <16 x float> %ZMM1_2 to i512
  %XMM1_2 = trunc i512 %116 to i128
  %117 = zext i64 %115 to i128
  %118 = and i128 %XMM1_2, -18446744073709551616
  %XMM1_3 = or i128 %117, %118
  %119 = bitcast <16 x float> %ZMM1_2 to i512
  %YMM1_2 = trunc i512 %119 to i256
  %120 = zext i128 %XMM1_3 to i256
  %121 = and i256 %YMM1_2, -340282366920938463463374607431768211456
  %YMM1_3 = or i256 %120, %121
  %122 = bitcast <16 x float> %ZMM1_2 to i512
  %123 = zext i128 %XMM1_3 to i512
  %124 = and i512 %122, -340282366920938463463374607431768211456
  %ZMM1_3 = or i512 %123, %124
  %RIP_18 = add i64 %RIP_17, 4
  %EIP_15 = trunc i64 %RIP_18 to i32
  %IP_15 = trunc i64 %RIP_18 to i16
  %125 = trunc i128 %XMM1_3 to i64
  %126 = bitcast i64 %125 to double
  %127 = trunc i128 %XMM0_4 to i64
  %128 = bitcast i64 %127 to double
  %ZF_02 = fcmp ueq double %126, %128
  %PF_03 = fcmp uno double %126, %128
  %CF_04 = fcmp ult double %126, %128
  %CtlSysEFLAGS_1 = load i32, i32* %CtlSysEFLAGS
  %129 = zext i1 %CF_04 to i32
  %130 = shl i32 %129, 0
  %131 = or i32 %130, %CtlSysEFLAGS_1
  %132 = zext i1 %PF_03 to i32
  %133 = shl i32 %132, 2
  %134 = or i32 %133, %131
  %135 = zext i1 false to i32
  %136 = shl i32 %135, 4
  %137 = or i32 %136, %134
  %138 = zext i1 %ZF_02 to i32
  %139 = shl i32 %138, 6
  %140 = or i32 %139, %137
  %141 = zext i1 false to i32
  %142 = shl i32 %141, 7
  %143 = or i32 %142, %140
  %144 = zext i1 false to i32
  %145 = shl i32 %144, 11
  %EFLAGS_2 = or i32 %145, %143
  %RIP_19 = add i64 %RIP_18, 6
  %EIP_16 = trunc i64 %RIP_19 to i32
  %IP_16 = trunc i64 %RIP_19 to i16
  %CC_BE_0 = or i1 %CF_04, %ZF_02
  store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
  store i32 %EFLAGS_2, i32* %EFLAGS
  store i32 4195559, i32* %EIP
  store i16 1255, i16* %IP
  store i64 %RBP_1, i64* %RBP
  store i64 4195559, i64* %RIP
  %146 = bitcast i128 %XMM0_4 to <4 x float>
  store <4 x float> %146, <4 x float>* %XMM0
  %147 = bitcast i128 %XMM1_3 to <4 x float>
  store <4 x float> %147, <4 x float>* %XMM1
  %148 = bitcast i256 %YMM0_4 to <8 x float>
  store <8 x float> %148, <8 x float>* %YMM0
  %149 = bitcast i256 %YMM1_3 to <8 x float>
  store <8 x float> %149, <8 x float>* %YMM1
  %150 = bitcast i512 %ZMM0_4 to <16 x float>
  store <16 x float> %150, <16 x float>* %ZMM0
  %151 = bitcast i512 %ZMM1_3 to <16 x float>
  store <16 x float> %151, <16 x float>* %ZMM1
  br i1 %CC_BE_0, label %bb_4004E7, label %bb_4004DB

bb_4004DB:                                        ; preds = %bb_4004C7
  %RIP_26 = add i64 4195547, 7
  %EIP_21 = trunc i64 %RIP_26 to i32
  %IP_21 = trunc i64 %RIP_26 to i16
  %RBP_3 = load i64, i64* %RBP
  %152 = add i64 %RBP_3, -28
  %153 = inttoptr i64 %152 to i32*
  store i32 -1, i32* %153, align 1
  %RIP_27 = add i64 %RIP_26, 5
  %EIP_22 = trunc i64 %RIP_27 to i32
  %IP_22 = trunc i64 %RIP_27 to i16
  store i32 4195566, i32* %EIP
  store i16 1262, i16* %IP
  store i64 %RBP_3, i64* %RBP
  store i64 4195566, i64* %RIP
  br label %bb_4004EE

bb_4004E7:                                        ; preds = %bb_4004C7
  %RIP_30 = add i64 4195559, 7
  %EIP_24 = trunc i64 %RIP_30 to i32
  %IP_24 = trunc i64 %RIP_30 to i16
  %RBP_4 = load i64, i64* %RBP
  %154 = add i64 %RBP_4, -28
  %155 = inttoptr i64 %154 to i32*
  store i32 1, i32* %155, align 1
  store i32 %EIP_24, i32* %EIP
  store i16 %IP_24, i16* %IP
  store i64 %RBP_4, i64* %RBP
  store i64 %RIP_30, i64* %RIP
  br label %bb_4004EE

bb_4004EE:                                        ; preds = %bb_4004E7, %bb_4004DB
  %RIP_40 = add i64 4195566, 5
  %EIP_32 = trunc i64 %RIP_40 to i32
  %IP_32 = trunc i64 %RIP_40 to i16
  store i32 4195571, i32* %EIP
  store i16 1267, i16* %IP
  store i64 4195571, i64* %RIP
  br label %bb_4004F3

bb_4004F3:                                        ; preds = %bb_4004EE, %bb_4004BB
  %RIP_32 = add i64 4195571, 3
  %EIP_25 = trunc i64 %RIP_32 to i32
  %IP_25 = trunc i64 %RIP_32 to i16
  %RBP_5 = load i64, i64* %RBP
  %156 = add i64 %RBP_5, -28
  %157 = inttoptr i64 %156 to i32*
  %EAX_0 = load i32, i32* %157, align 1
  %RAX_0 = load i64, i64* %RAX
  %RAX_1 = zext i32 %EAX_0 to i64
  %AX_0 = trunc i32 %EAX_0 to i16
  %AL_0 = trunc i32 %EAX_0 to i8
  %158 = lshr i32 %EAX_0, 8
  %AH_0 = trunc i32 %158 to i8
  %RIP_33 = add i64 %RIP_32, 2
  %EIP_26 = trunc i64 %RIP_33 to i32
  %IP_26 = trunc i64 %RIP_33 to i16
  %RCX_0 = load i64, i64* %RCX
  %RCX_1 = zext i32 %EAX_0 to i64
  %CX_0 = trunc i32 %EAX_0 to i16
  %CL_0 = trunc i32 %EAX_0 to i8
  %159 = lshr i32 %EAX_0, 8
  %CH_0 = trunc i32 %159 to i8
  %RIP_34 = add i64 %RIP_33, 3
  %EIP_27 = trunc i64 %RIP_34 to i32
  %IP_27 = trunc i64 %RIP_34 to i16
  %ECX_0 = sub i32 %EAX_0, -1
  %RCX_2 = zext i32 %ECX_0 to i64
  %CX_1 = trunc i32 %ECX_0 to i16
  %CL_1 = trunc i32 %ECX_0 to i8
  %160 = lshr i32 %ECX_0, 8
  %CH_1 = trunc i32 %160 to i8
  %EFLAGS_3 = load i32, i32* %EFLAGS
  %RIP_35 = add i64 %RIP_34, 3
  %EIP_28 = trunc i64 %RIP_35 to i32
  %IP_28 = trunc i64 %RIP_35 to i16
  %161 = add i64 %RBP_5, -36
  %162 = inttoptr i64 %161 to i32*
  store i32 %EAX_0, i32* %162, align 1
  %RIP_36 = add i64 %RIP_35, 3
  %EIP_29 = trunc i64 %RIP_36 to i32
  %IP_29 = trunc i64 %RIP_36 to i16
  %163 = add i64 %RBP_5, -40
  %164 = inttoptr i64 %163 to i32*
  store i32 %ECX_0, i32* %164, align 1
  %RIP_37 = add i64 %RIP_36, 6
  %EIP_30 = trunc i64 %RIP_37 to i32
  %IP_30 = trunc i64 %RIP_37 to i16
  %ZF_05 = icmp eq i32 %ECX_0, 0
  %SF_0 = icmp slt i32 %ECX_0, 0
  %165 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_0, i32 -1)
  %OF_0 = extractvalue { i32, i1 } %165, 1
  %166 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_0, i32 -1)
  %CF_06 = extractvalue { i32, i1 } %166, 1
  %167 = trunc i32 %ECX_0 to i8
  %168 = call i8 @llvm.ctpop.i8(i8 %167)
  %169 = trunc i8 %168 to i1
  %PF_07 = icmp eq i1 %169, false
  %CtlSysEFLAGS_2 = load i32, i32* %CtlSysEFLAGS
  %170 = zext i1 %CF_06 to i32
  %171 = shl i32 %170, 0
  %172 = or i32 %171, %CtlSysEFLAGS_2
  %173 = zext i1 %PF_07 to i32
  %174 = shl i32 %173, 2
  %175 = or i32 %174, %172
  %176 = zext i1 false to i32
  %177 = shl i32 %176, 4
  %178 = or i32 %177, %175
  %179 = zext i1 %ZF_05 to i32
  %180 = shl i32 %179, 6
  %181 = or i32 %180, %178
  %182 = zext i1 %SF_0 to i32
  %183 = shl i32 %182, 7
  %184 = or i32 %183, %181
  %185 = zext i1 %OF_0 to i32
  %186 = shl i32 %185, 11
  %EFLAGS_4 = or i32 %186, %184
  %187 = lshr i32 %EFLAGS_4, 6
  %ZF_1 = trunc i32 %187 to i1
  store i8 %AH_0, i8* %AH
  store i8 %AL_0, i8* %AL
  store i16 %AX_0, i16* %AX
  store i8 %CH_1, i8* %CH
  store i8 %CL_1, i8* %CL
  store i16 %CX_1, i16* %CX
  store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
  store i32 %EAX_0, i32* %EAX
  store i32 %ECX_0, i32* %ECX
  store i32 %EFLAGS_4, i32* %EFLAGS
  store i32 4195616, i32* %EIP
  store i16 1312, i16* %IP
  store i64 %RAX_1, i64* %RAX
  store i64 %RBP_5, i64* %RBP
  store i64 %RCX_2, i64* %RCX
  store i64 4195616, i64* %RIP
  br i1 %ZF_1, label %bb_400520, label %bb_400507

bb_400507:                                        ; preds = %bb_4004F3
  %RIP_43 = add i64 4195591, 5
  %EIP_34 = trunc i64 %RIP_43 to i32
  %IP_34 = trunc i64 %RIP_43 to i16
  store i32 4195596, i32* %EIP
  store i16 1292, i16* %IP
  store i64 4195596, i64* %RIP
  br label %bb_40050C

bb_40050C:                                        ; preds = %bb_400507
  %RIP_50 = add i64 4195596, 3
  %EIP_39 = trunc i64 %RIP_50 to i32
  %IP_39 = trunc i64 %RIP_50 to i16
  %RBP_7 = load i64, i64* %RBP
  %188 = add i64 %RBP_7, -36
  %189 = inttoptr i64 %188 to i32*
  %EAX_1 = load i32, i32* %189, align 1
  %RAX_2 = load i64, i64* %RAX
  %RAX_3 = zext i32 %EAX_1 to i64
  %AX_1 = trunc i32 %EAX_1 to i16
  %AL_1 = trunc i32 %EAX_1 to i8
  %190 = lshr i32 %EAX_1, 8
  %AH_1 = trunc i32 %190 to i8
  %RIP_51 = add i64 %RIP_50, 3
  %EIP_40 = trunc i64 %RIP_51 to i32
  %IP_40 = trunc i64 %RIP_51 to i16
  %EAX_2 = sub i32 %EAX_1, 1
  %RAX_4 = zext i32 %EAX_2 to i64
  %AX_2 = trunc i32 %EAX_2 to i16
  %AL_2 = trunc i32 %EAX_2 to i8
  %191 = lshr i32 %EAX_2, 8
  %AH_2 = trunc i32 %191 to i8
  %EFLAGS_5 = load i32, i32* %EFLAGS
  %RIP_52 = add i64 %RIP_51, 3
  %EIP_41 = trunc i64 %RIP_52 to i32
  %IP_41 = trunc i64 %RIP_52 to i16
  %192 = add i64 %RBP_7, -44
  %193 = inttoptr i64 %192 to i32*
  store i32 %EAX_2, i32* %193, align 1
  %RIP_53 = add i64 %RIP_52, 6
  %EIP_42 = trunc i64 %RIP_53 to i32
  %IP_42 = trunc i64 %RIP_53 to i16
  %ZF_08 = icmp eq i32 %EAX_2, 0
  %SF_09 = icmp slt i32 %EAX_2, 0
  %194 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_1, i32 1)
  %OF_010 = extractvalue { i32, i1 } %194, 1
  %195 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_1, i32 1)
  %CF_011 = extractvalue { i32, i1 } %195, 1
  %196 = trunc i32 %EAX_2 to i8
  %197 = call i8 @llvm.ctpop.i8(i8 %196)
  %198 = trunc i8 %197 to i1
  %PF_012 = icmp eq i1 %198, false
  %CtlSysEFLAGS_3 = load i32, i32* %CtlSysEFLAGS
  %199 = zext i1 %CF_011 to i32
  %200 = shl i32 %199, 0
  %201 = or i32 %200, %CtlSysEFLAGS_3
  %202 = zext i1 %PF_012 to i32
  %203 = shl i32 %202, 2
  %204 = or i32 %203, %201
  %205 = zext i1 false to i32
  %206 = shl i32 %205, 4
  %207 = or i32 %206, %204
  %208 = zext i1 %ZF_08 to i32
  %209 = shl i32 %208, 6
  %210 = or i32 %209, %207
  %211 = zext i1 %SF_09 to i32
  %212 = shl i32 %211, 7
  %213 = or i32 %212, %210
  %214 = zext i1 %OF_010 to i32
  %215 = shl i32 %214, 11
  %EFLAGS_6 = or i32 %215, %213
  %216 = lshr i32 %EFLAGS_6, 6
  %ZF_113 = trunc i32 %216 to i1
  store i8 %AH_2, i8* %AH
  store i8 %AL_2, i8* %AL
  store i16 %AX_2, i16* %AX
  store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
  store i32 %EAX_2, i32* %EAX
  store i32 %EFLAGS_6, i32* %EFLAGS
  store i32 4195628, i32* %EIP
  store i16 1324, i16* %IP
  store i64 %RAX_4, i64* %RAX
  store i64 %RBP_7, i64* %RBP
  store i64 4195628, i64* %RIP
  br i1 %ZF_113, label %bb_40052C, label %bb_40051B

bb_40051B:                                        ; preds = %bb_40050C
  %RIP_61 = add i64 4195611, 5
  %EIP_48 = trunc i64 %RIP_61 to i32
  %IP_48 = trunc i64 %RIP_61 to i16
  store i32 4195640, i32* %EIP
  store i16 1336, i16* %IP
  store i64 4195640, i64* %RIP
  br label %bb_400538

bb_400520:                                        ; preds = %bb_4004F3
  %RIP_46 = add i64 4195616, 7
  %EIP_36 = trunc i64 %RIP_46 to i32
  %IP_36 = trunc i64 %RIP_46 to i16
  %RBP_6 = load i64, i64* %RBP
  %217 = add i64 %RBP_6, -32
  %218 = inttoptr i64 %217 to i32*
  store i32 -1, i32* %218, align 1
  %RIP_47 = add i64 %RIP_46, 5
  %EIP_37 = trunc i64 %RIP_47 to i32
  %IP_37 = trunc i64 %RIP_47 to i16
  store i32 4195647, i32* %EIP
  store i16 1343, i16* %IP
  store i64 %RBP_6, i64* %RBP
  store i64 4195647, i64* %RIP
  br label %bb_40053F

bb_40052C:                                        ; preds = %bb_40050C
  %RIP_64 = add i64 4195628, 7
  %EIP_50 = trunc i64 %RIP_64 to i32
  %IP_50 = trunc i64 %RIP_64 to i16
  %RBP_10 = load i64, i64* %RBP
  %219 = add i64 %RBP_10, -32
  %220 = inttoptr i64 %219 to i32*
  store i32 1, i32* %220, align 1
  %RIP_65 = add i64 %RIP_64, 5
  %EIP_51 = trunc i64 %RIP_65 to i32
  %IP_51 = trunc i64 %RIP_65 to i16
  store i32 4195647, i32* %EIP
  store i16 1343, i16* %IP
  store i64 %RBP_10, i64* %RBP
  store i64 4195647, i64* %RIP
  br label %bb_40053F

bb_400538:                                        ; preds = %bb_40051B
  %RIP_68 = add i64 4195640, 7
  %EIP_53 = trunc i64 %RIP_68 to i32
  %IP_53 = trunc i64 %RIP_68 to i16
  %RBP_11 = load i64, i64* %RBP
  %221 = add i64 %RBP_11, -32
  %222 = inttoptr i64 %221 to i32*
  store i32 0, i32* %222, align 1
  store i32 %EIP_53, i32* %EIP
  store i16 %IP_53, i16* %IP
  store i64 %RBP_11, i64* %RBP
  store i64 %RIP_68, i64* %RIP
  br label %bb_40053F

bb_40053F:                                        ; preds = %bb_400538, %bb_40052C, %bb_400520
  %RIP_56 = add i64 4195647, 3
  %EIP_44 = trunc i64 %RIP_56 to i32
  %IP_44 = trunc i64 %RIP_56 to i16
  %RBP_8 = load i64, i64* %RBP
  %223 = add i64 %RBP_8, -32
  %224 = inttoptr i64 %223 to i32*
  %EAX_3 = load i32, i32* %224, align 1
  %RAX_5 = load i64, i64* %RAX
  %RAX_6 = zext i32 %EAX_3 to i64
  %AX_3 = trunc i32 %EAX_3 to i16
  %AL_3 = trunc i32 %EAX_3 to i8
  %225 = lshr i32 %EAX_3, 8
  %AH_3 = trunc i32 %225 to i8
  %RIP_57 = add i64 %RIP_56, 1
  %EIP_45 = trunc i64 %RIP_57 to i32
  %IP_45 = trunc i64 %RIP_57 to i16
  %RSP_2 = load i64, i64* %RSP
  %RSP_3 = add i64 %RSP_2, 8
  %ESP_1 = trunc i64 %RSP_3 to i32
  %SP_1 = trunc i64 %RSP_3 to i16
  %SPL_1 = trunc i64 %RSP_3 to i8
  %226 = sub i64 %RSP_3, 8
  %227 = inttoptr i64 %226 to i64*
  %RBP_9 = load i64, i64* %227, align 1
  %EBP_1 = trunc i64 %RBP_9 to i32
  %BP_1 = trunc i64 %RBP_9 to i16
  %BPL_1 = trunc i64 %RBP_9 to i8
  %RIP_58 = add i64 %RIP_57, 1
  %EIP_46 = trunc i64 %RIP_58 to i32
  %IP_46 = trunc i64 %RIP_58 to i16
  %RSP_4 = add i64 %RSP_3, 8
  %228 = inttoptr i64 %RSP_3 to i64*
  %RIP_59 = load i64, i64* %228
  %ESP_2 = trunc i64 %RSP_4 to i32
  %SP_2 = trunc i64 %RSP_4 to i16
  %SPL_2 = trunc i64 %RSP_4 to i8
  %EIP_47 = trunc i64 %RIP_59 to i32
  %IP_47 = trunc i64 %RIP_59 to i16
  store i8 %AH_3, i8* %AH
  store i8 %AL_3, i8* %AL
  store i16 %AX_3, i16* %AX
  store i16 %BP_1, i16* %BP
  store i8 %BPL_1, i8* %BPL
  store i32 %EAX_3, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 %EIP_47, i32* %EIP
  store i32 %ESP_2, i32* %ESP
  store i16 %IP_47, i16* %IP
  store i64 %RAX_6, i64* %RAX
  store i64 %RBP_9, i64* %RBP
  store i64 %RIP_59, i64* %RIP
  store i64 %RSP_4, i64* %RSP
  store i16 %SP_2, i16* %SP
  store i8 %SPL_2, i8* %SPL
  br label %exit_fn_400480
}

; Function Attrs: noreturn nounwind
declare void @llvm.trap() #0

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32) #1

; Function Attrs: nounwind readnone speculatable
declare { i32, i1 } @llvm.usub.with.overflow.i32(i32, i32) #1

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
