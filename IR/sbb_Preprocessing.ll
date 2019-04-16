entry_fn_400480:
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
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%2 = bitcast <16 x float> %ZMM0_init to i512
%3 = trunc i512 %2 to i128
%XMM0_init = %ZMM0_ptr
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%4 = bitcast <16 x float> %ZMM0_init to i512
%5 = trunc i512 %4 to i256
%YMM0_init = %ZMM0_ptr
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%6 = bitcast <16 x float> %ZMM1_init to i512
%7 = trunc i512 %6 to i128
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = bitcast <16 x float> %ZMM1_init to i512
%9 = trunc i512 %8 to i256
%YMM1_init = %ZMM1_ptr
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %RCX_ptr
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %RCX_init, 8
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%CtlSysEFLAGS = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
br label %bb_400480
exit_fn_400480:                                   ; preds = %bb_400480
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
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%20 = sub i64 %RSP_0, 8
%21 = inttoptr i64 %20 to i64*
store i64 %RBP_0, i64* %21, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195460
%EIP_1 = 4195460
%EBP_0 = %RSP-8
%RIP_3 = 4195462
%EIP_2 = 4195462
%RAX_0 = %RAX
%EAX_0 = %RAX
%EAX_1 = xor i32 %EAX_0, %EAX_0
%RAX_1 = %EAX_1
%22 = lshr i32 %EAX_1, 8
%EFLAGS_0 = %EFLAGS
%RIP_4 = 4195470
%EIP_3 = 4195470
%23 = add i64 %RIP_4, 202
%24 = inttoptr i64 %23 to double*
%25 = load double, double* %24, align 1
%26 = bitcast double %25 to i64
%ZMM0_0 = %ZMM0
%27 = bitcast <16 x float> %ZMM0_0 to i512
%XMM0_0 = %ZMM0
%28 = zext i64 %26 to i128
%29 = and i128 %XMM0_0, -18446744073709551616
%XMM0_1 = or i128 %28, %29
%30 = bitcast <16 x float> %ZMM0_0 to i512
%YMM0_0 = %ZMM0
%31 = zext i128 %XMM0_1 to i256
%32 = and i256 %YMM0_0, -340282366920938463463374607431768211456
%YMM0_1 = or i256 %31, %32
%33 = bitcast <16 x float> %ZMM0_0 to i512
%34 = zext i128 %XMM0_1 to i512
%35 = and i512 %33, -340282366920938463463374607431768211456
%ZMM0_1 = or i512 %34, %35
%RIP_5 = 4195478
%EIP_4 = 4195478
%36 = add i64 %RIP_5, 202
%37 = inttoptr i64 %36 to double*
%38 = load double, double* %37, align 1
%39 = bitcast double %38 to i64
%ZMM1_0 = %ZMM1
%40 = bitcast <16 x float> %ZMM1_0 to i512
%XMM1_0 = %ZMM1
%41 = zext i64 %39 to i128
%42 = and i128 %XMM1_0, -18446744073709551616
%XMM1_1 = or i128 %41, %42
%43 = bitcast <16 x float> %ZMM1_0 to i512
%YMM1_0 = %ZMM1
%44 = zext i128 %XMM1_1 to i256
%45 = and i256 %YMM1_0, -340282366920938463463374607431768211456
%YMM1_1 = or i256 %44, %45
%46 = bitcast <16 x float> %ZMM1_0 to i512
%47 = zext i128 %XMM1_1 to i512
%48 = and i512 %46, -340282366920938463463374607431768211456
%ZMM1_1 = or i512 %47, %48
%RIP_6 = 4195485
%EIP_5 = 4195485
%49 = add i64 %RSP_1, -4
%50 = inttoptr i64 %49 to i32*
store i32 0, i32* %50, align 1
%RIP_7 = 4195492
%EIP_6 = 4195492
%51 = add i64 %RSP_1, -8
%52 = inttoptr i64 %51 to i32*
store i32 10, i32* %52, align 1
%RIP_8 = 4195497
%EIP_7 = 4195497
%53 = trunc i128 %XMM1_1 to i64
%54 = bitcast i64 %53 to double
%55 = add i64 %RSP_1, -16
%56 = inttoptr i64 %55 to double*
store double %54, double* %56, align 1
%RIP_9 = 4195502
%EIP_8 = 4195502
%57 = trunc i128 %XMM0_1 to i64
%58 = bitcast i64 %57 to double
%59 = add i64 %RSP_1, -24
%60 = inttoptr i64 %59 to double*
store double %58, double* %60, align 1
%RIP_10 = 4195505
%EIP_9 = 4195505
%61 = add i64 %RSP_1, -8
%62 = inttoptr i64 %61 to i32*
%ECX_0 = %RSP-16
%RCX_0 = %RCX
%RCX_1 = %RSP-16
%63 = lshr i32 %ECX_0, 8
%RIP_11 = 4195509
%EIP_10 = 4195509
%64 = sitofp i32 %ECX_0 to double
%65 = bitcast double %64 to i64
%66 = zext i64 %65 to i128
%67 = and i128 %XMM0_1, -18446744073709551616
%XMM0_2 = or i128 %66, %67
%68 = zext i128 %XMM0_2 to i256
%69 = and i256 %YMM0_1, -340282366920938463463374607431768211456
%YMM0_2 = or i256 %68, %69
%70 = zext i128 %XMM0_2 to i512
%71 = and i512 %ZMM0_1, -340282366920938463463374607431768211456
%ZMM0_2 = or i512 %70, %71
%RIP_12 = 4195514
%EIP_11 = 4195514
%72 = trunc i128 %XMM0_2 to i64
%73 = bitcast i64 %72 to double
%74 = add i64 %RSP_1, -24
%75 = inttoptr i64 %74 to double*
%76 = load double, double* %75, align 1
%77 = fmul double %73, %76
%78 = bitcast double %77 to i64
%79 = zext i64 %78 to i128
%80 = and i128 %XMM0_2, -18446744073709551616
%XMM0_3 = or i128 %79, %80
%81 = zext i128 %XMM0_3 to i256
%82 = and i256 %YMM0_2, -340282366920938463463374607431768211456
%YMM0_3 = or i256 %81, %82
%83 = zext i128 %XMM0_3 to i512
%84 = and i512 %ZMM0_2, -340282366920938463463374607431768211456
%ZMM0_3 = or i512 %83, %84
%RIP_13 = 4195519
%EIP_12 = 4195519
%85 = trunc i128 %XMM0_3 to i64
%86 = bitcast i64 %85 to double
%87 = add i64 %RSP_1, -16
%88 = inttoptr i64 %87 to double*
%89 = load double, double* %88, align 1
%90 = fadd double %86, %89
%91 = bitcast double %90 to i64
%92 = zext i64 %91 to i128
%93 = and i128 %XMM0_3, -18446744073709551616
%XMM0_4 = or i128 %92, %93
%94 = zext i128 %XMM0_4 to i256
%95 = and i256 %YMM0_3, -340282366920938463463374607431768211456
%YMM0_4 = or i256 %94, %95
%96 = zext i128 %XMM0_4 to i512
%97 = and i512 %ZMM0_3, -340282366920938463463374607431768211456
%ZMM0_4 = or i512 %96, %97
%RIP_14 = 4195524
%EIP_13 = 4195524
%98 = trunc i128 %XMM0_4 to i64
%99 = bitcast i64 %98 to double
%100 = add i64 %RSP_1, -32
%101 = inttoptr i64 %100 to double*
store double %99, double* %101, align 1
%RIP_15 = 4195525
%EIP_14 = 4195525
%RSP_2 = %RSP
%ESP_1 = %RSP
%102 = sub i64 %RSP_2, 8
%103 = inttoptr i64 %102 to i64*
%RBP_1 = %RSP-8
%EBP_1 = %RSP-8
%RIP_16 = 4195526
%EIP_15 = 4195526
%RSP_3 = %RSP+8
%104 = inttoptr i64 %RSP_2 to i64*
%RIP_17 = %RSP
%ESP_2 = %RSP+8
%EIP_16 = %RSP
%ZF_0 = icmp eq i32 %EAX_1, 0
%SF_0 = icmp slt i32 %EAX_1, 0
%105 = trunc i32 %EAX_1 to i8
%106 = call i8 @llvm.ctpop.i8(i8 %105)
%107 = trunc i8 %106 to i1
%PF_0 = icmp eq i1 %107, false
%CtlSysEFLAGS_0 = load i32, i32* %CtlSysEFLAGS
%108 = zext i1 false to i32
%109 = shl i32 %108, 0
%110 = or i32 %109, %CtlSysEFLAGS_0
%111 = zext i1 %PF_0 to i32
%112 = shl i32 %111, 2
%113 = or i32 %112, %110
%114 = zext i1 false to i32
%115 = shl i32 %114, 4
%116 = or i32 %115, %113
%117 = zext i1 %ZF_0 to i32
%118 = shl i32 %117, 6
%119 = or i32 %118, %116
%120 = zext i1 %SF_0 to i32
%121 = shl i32 %120, 7
%122 = or i32 %121, %119
%123 = zext i1 false to i32
%124 = shl i32 %123, 11
%EFLAGS_1 = or i32 %122, %124
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %EAX_1, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %ECX_0, i32* %ECX
store i32 %EFLAGS_1, i32* %EFLAGS
store i32 %EIP_16, i32* %EIP
store i32 %ESP_2, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 %RCX_1, i64* %RCX
store i64 %RIP_17, i64* %RIP
store i64 %RSP_3, i64* %RSP
%125 = bitcast i128 %XMM0_4 to <4 x float>
store <4 x float> %125, <4 x float>* %XMM0
%126 = bitcast i128 %XMM1_1 to <4 x float>
store <4 x float> %126, <4 x float>* %XMM1
%127 = bitcast i256 %YMM0_4 to <8 x float>
store <8 x float> %127, <8 x float>* %YMM0
%128 = bitcast i256 %YMM1_1 to <8 x float>
store <8 x float> %128, <8 x float>* %YMM1
%129 = bitcast i512 %ZMM0_4 to <16 x float>
store <16 x float> %129, <16 x float>* %ZMM0
%130 = bitcast i512 %ZMM1_1 to <16 x float>
store <16 x float> %130, <16 x float>* %ZMM1
br label %exit_fn_400480

