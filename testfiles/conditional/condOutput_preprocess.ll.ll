define void @fn_400480(%regset* noalias nocapture) {
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
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%1 = bitcast <16 x float> %ZMM0_init to i512
%2 = trunc i512 %1 to i128
%XMM0_init = %ZMM0_ptr
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%3 = bitcast <16 x float> %ZMM0_init to i512
%4 = trunc i512 %3 to i256
%YMM0_init = %ZMM0_ptr
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%5 = bitcast <16 x float> %ZMM1_init to i512
%6 = trunc i512 %5 to i128
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%7 = bitcast <16 x float> %ZMM1_init to i512
%8 = trunc i512 %7 to i256
%YMM1_init = %ZMM1_ptr
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%CtlSysEFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 1
%CtlSysEFLAGS_init = load i32, i32* %CtlSysEFLAGS_ptr
%CtlSysEFLAGS = alloca i32
store i32 %CtlSysEFLAGS_init, i32* %CtlSysEFLAGS
%EFLAGS_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 3
%EFLAGS_init = %EFLAGS_ptr
%EFLAGS = alloca i32
store i32 %EFLAGS_init, i32* %EFLAGS
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%9 = lshr i64 %RAX_init, 8
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %RCX_ptr
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %RCX_init, 8
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
%RIP_3 = 4195468
%EIP_2 = 4195468
%22 = add i64 %RIP_3, 332
%23 = inttoptr i64 %22 to double*
%24 = load double, double* %23, align 1
%25 = bitcast double %24 to i64
%ZMM0_0 = %ZMM0
%26 = bitcast <16 x float> %ZMM0_0 to i512
%XMM0_0 = %ZMM0
%27 = zext i64 %25 to i128
%28 = and i128 %XMM0_0, -18446744073709551616
%XMM0_1 = or i128 %27, %28
%29 = bitcast <16 x float> %ZMM0_0 to i512
%YMM0_0 = %ZMM0
%30 = zext i128 %XMM0_1 to i256
%31 = and i256 %YMM0_0, -340282366920938463463374607431768211456
%YMM0_1 = or i256 %30, %31
%32 = bitcast <16 x float> %ZMM0_0 to i512
%33 = zext i128 %XMM0_1 to i512
%34 = and i512 %32, -340282366920938463463374607431768211456
%ZMM0_1 = or i512 %33, %34
%RIP_4 = 4195476
%EIP_3 = 4195476
%35 = add i64 %RIP_4, 332
%36 = inttoptr i64 %35 to double*
%37 = load double, double* %36, align 1
%38 = bitcast double %37 to i64
%ZMM1_0 = %ZMM1
%39 = bitcast <16 x float> %ZMM1_0 to i512
%XMM1_0 = %ZMM1
%40 = zext i64 %38 to i128
%41 = and i128 %XMM1_0, -18446744073709551616
%XMM1_1 = or i128 %40, %41
%42 = bitcast <16 x float> %ZMM1_0 to i512
%YMM1_0 = %ZMM1
%43 = zext i128 %XMM1_1 to i256
%44 = and i256 %YMM1_0, -340282366920938463463374607431768211456
%YMM1_1 = or i256 %43, %44
%45 = bitcast <16 x float> %ZMM1_0 to i512
%46 = zext i128 %XMM1_1 to i512
%47 = and i512 %45, -340282366920938463463374607431768211456
%ZMM1_1 = or i512 %46, %47
%RIP_5 = 4195483
%EIP_4 = 4195483
%48 = add i64 %RSP_1, -4
%49 = inttoptr i64 %48 to i32*
store i32 0, i32* %49, align 1
%RIP_6 = 4195488
%EIP_5 = 4195488
%50 = trunc i128 %XMM1_1 to i64
%51 = bitcast i64 %50 to double
%52 = add i64 %RSP_1, -16
%53 = inttoptr i64 %52 to double*
store double %51, double* %53, align 1
%RIP_7 = 4195493
%EIP_6 = 4195493
%54 = trunc i128 %XMM0_1 to i64
%55 = bitcast i64 %54 to double
%56 = add i64 %RSP_1, -24
%57 = inttoptr i64 %56 to double*
store double %55, double* %57, align 1
%RIP_8 = 4195498
%EIP_7 = 4195498
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
%RIP_9 = 4195503
%EIP_8 = 4195503
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
%EFLAGS_0 = or i32 %87, %89
%RIP_10 = 4195509
%EIP_9 = 4195509
%CC_NE_0 = xor i1 %ZF_0, true
store i32 %CtlSysEFLAGS_0, i32* %CtlSysEFLAGS
store i32 %EBP_0, i32* %EBP
store i32 %EFLAGS_0, i32* %EFLAGS
store i32 4195527, i32* %EIP
store i32 %ESP_0, i32* %ESP
store i64 %RSP_1, i64* %RBP
store i64 4195527, i64* %RIP
store i64 %RSP_1, i64* %RSP
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
%RIP_13 = 4195515
%EIP_11 = 4195515
%EFLAGS_1 = %EFLAGS_0
%96 = lshr i32 %EFLAGS_1, 2
%PF_01 = trunc i32 %96 to i1
store i32 %EFLAGS_1, i32* %EFLAGS
store i32 4195527, i32* %EIP
store i64 4195527, i64* %RIP
br i1 %PF_01, label %bb_4004C7, label %bb_4004BB
bb_4004BB:                                        ; preds = %bb_4004B5
%RIP_22 = 4195522
%EIP_18 = 4195522
%RBP_2 = %RSP-8
%97 = add i64 %RBP_2, -28
%98 = inttoptr i64 %97 to i32*
store i32 0, i32* %98, align 1
%RIP_23 = 4195527
%EIP_19 = 4195527
store i32 4195571, i32* %EIP
store i64 %RBP_2, i64* %RBP
store i64 4195571, i64* %RIP
br label %bb_4004F3
bb_4004C7:                                        ; preds = %bb_4004B5, %bb_400480
%RIP_16 = 4195532
%EIP_13 = 4195532
%RBP_1 = %RSP-8
%99 = add i64 %RBP_1, -16
%100 = inttoptr i64 %99 to double*
%101 = load double, double* %100, align 1
%102 = bitcast double %101 to i64
%ZMM0_3 = %ZMM0_2
%103 = bitcast <16 x float> %ZMM0_3 to i512
%XMM0_3 = %ZMM0_2
%104 = zext i64 %102 to i128
%105 = and i128 %XMM0_3, -18446744073709551616
%XMM0_4 = or i128 %104, %105
%106 = bitcast <16 x float> %ZMM0_3 to i512
%YMM0_3 = %ZMM0_2
%107 = zext i128 %XMM0_4 to i256
%108 = and i256 %YMM0_3, -340282366920938463463374607431768211456
%YMM0_4 = or i256 %107, %108
%109 = bitcast <16 x float> %ZMM0_3 to i512
%110 = zext i128 %XMM0_4 to i512
%111 = and i512 %109, -340282366920938463463374607431768211456
%ZMM0_4 = or i512 %110, %111
%RIP_17 = 4195537
%EIP_14 = 4195537
%112 = add i64 %RBP_1, -24
%113 = inttoptr i64 %112 to double*
%114 = load double, double* %113, align 1
%115 = bitcast double %114 to i64
%ZMM1_2 = %ZMM1_1
%116 = bitcast <16 x float> %ZMM1_2 to i512
%XMM1_2 = %ZMM1_1
%117 = zext i64 %115 to i128
%118 = and i128 %XMM1_2, -18446744073709551616
%XMM1_3 = or i128 %117, %118
%119 = bitcast <16 x float> %ZMM1_2 to i512
%YMM1_2 = %ZMM1_1
%120 = zext i128 %XMM1_3 to i256
%121 = and i256 %YMM1_2, -340282366920938463463374607431768211456
%YMM1_3 = or i256 %120, %121
%122 = bitcast <16 x float> %ZMM1_2 to i512
%123 = zext i128 %XMM1_3 to i512
%124 = and i512 %122, -340282366920938463463374607431768211456
%ZMM1_3 = or i512 %123, %124
%RIP_18 = 4195541
%EIP_15 = 4195541
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
%EFLAGS_2 = or i32 %143, %145
%RIP_19 = 4195547
%EIP_16 = 4195547
%CC_BE_0 = or i1 %CF_04, %ZF_02
store i32 %CtlSysEFLAGS_1, i32* %CtlSysEFLAGS
store i32 %EFLAGS_2, i32* %EFLAGS
store i32 4195559, i32* %EIP
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
%RIP_26 = 4195554
%EIP_21 = 4195554
%RBP_3 = %RSP-8
%152 = add i64 %RBP_3, -28
%153 = inttoptr i64 %152 to i32*
store i32 -1, i32* %153, align 1
%RIP_27 = 4195559
%EIP_22 = 4195559
store i32 4195566, i32* %EIP
store i64 %RBP_3, i64* %RBP
store i64 4195566, i64* %RIP
br label %bb_4004EE
bb_4004E7:                                        ; preds = %bb_4004C7
%RIP_30 = 4195566
%EIP_24 = 4195566
%RBP_4 = %RSP-8
%154 = add i64 %RBP_4, -28
%155 = inttoptr i64 %154 to i32*
store i32 1, i32* %155, align 1
store i32 %EIP_24, i32* %EIP
store i64 %RBP_4, i64* %RBP
store i64 %RIP_30, i64* %RIP
br label %bb_4004EE
bb_4004EE:                                        ; preds = %bb_4004E7, %bb_4004DB
%RIP_40 = 4195571
%EIP_32 = 4195571
store i32 4195571, i32* %EIP
store i64 4195571, i64* %RIP
br label %bb_4004F3
bb_4004F3:                                        ; preds = %bb_4004EE, %bb_4004BB
%RIP_32 = 4195574
%EIP_25 = 4195574
%RBP_5 = %RSP-8
%156 = add i64 %RBP_5, -28
%157 = inttoptr i64 %156 to i32*
%EAX_0 = %RSP-36
%RAX_0 = %RAX
%RAX_1 = %RSP-36
%158 = lshr i32 %EAX_0, 8
%RIP_33 = 4195576
%EIP_26 = 4195576
%RCX_0 = %RCX
%RCX_1 = %RSP-36
%159 = lshr i32 %EAX_0, 8
%RIP_34 = 4195579
%EIP_27 = 4195579
%ECX_0 = %RSP-35
%RCX_2 = %RSP-35
%160 = lshr i32 %ECX_0, 8
%EFLAGS_3 = %EFLAGS_2
%RIP_35 = 4195582
%EIP_28 = 4195582
%161 = add i64 %RBP_5, -36
%162 = inttoptr i64 %161 to i32*
store i32 %EAX_0, i32* %162, align 1
%RIP_36 = 4195585
%EIP_29 = 4195585
%163 = add i64 %RBP_5, -40
%164 = inttoptr i64 %163 to i32*
store i32 %ECX_0, i32* %164, align 1
%RIP_37 = 4195591
%EIP_30 = 4195591
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
%EFLAGS_4 = or i32 %184, %186
%187 = lshr i32 %EFLAGS_4, 6
%ZF_1 = trunc i32 %187 to i1
store i32 %CtlSysEFLAGS_2, i32* %CtlSysEFLAGS
store i32 %EAX_0, i32* %EAX
store i32 %ECX_0, i32* %ECX
store i32 %EFLAGS_4, i32* %EFLAGS
store i32 4195616, i32* %EIP
store i64 %RAX_1, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 %RCX_2, i64* %RCX
store i64 4195616, i64* %RIP
br i1 %ZF_1, label %bb_400520, label %bb_400507
bb_400507:                                        ; preds = %bb_4004F3
%RIP_43 = 4195596
%EIP_34 = 4195596
store i32 4195596, i32* %EIP
store i64 4195596, i64* %RIP
br label %bb_40050C
bb_40050C:                                        ; preds = %bb_400507
%RIP_50 = 4195599
%EIP_39 = 4195599
%RBP_7 = %RSP-8
%188 = add i64 %RBP_7, -36
%189 = inttoptr i64 %188 to i32*
%EAX_1 = %RSP-44
%RAX_2 = %RSP-36
%RAX_3 = %RSP-44
%190 = lshr i32 %EAX_1, 8
%RIP_51 = 4195602
%EIP_40 = 4195602
%EAX_2 = %RSP-45
%RAX_4 = %RSP-45
%191 = lshr i32 %EAX_2, 8
%EFLAGS_5 = %EFLAGS_4
%RIP_52 = 4195605
%EIP_41 = 4195605
%192 = add i64 %RBP_7, -44
%193 = inttoptr i64 %192 to i32*
store i32 %EAX_2, i32* %193, align 1
%RIP_53 = 4195611
%EIP_42 = 4195611
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
%EFLAGS_6 = or i32 %213, %215
%216 = lshr i32 %EFLAGS_6, 6
%ZF_113 = trunc i32 %216 to i1
store i32 %CtlSysEFLAGS_3, i32* %CtlSysEFLAGS
store i32 %EAX_2, i32* %EAX
store i32 %EFLAGS_6, i32* %EFLAGS
store i32 4195628, i32* %EIP
store i64 %RAX_4, i64* %RAX
store i64 %RBP_7, i64* %RBP
store i64 4195628, i64* %RIP
br i1 %ZF_113, label %bb_40052C, label %bb_40051B
bb_40051B:                                        ; preds = %bb_40050C
%RIP_61 = 4195616
%EIP_48 = 4195616
store i32 4195640, i32* %EIP
store i64 4195640, i64* %RIP
br label %bb_400538
bb_400520:                                        ; preds = %bb_4004F3
%RIP_46 = 4195623
%EIP_36 = 4195623
%RBP_6 = %RSP-8
%217 = add i64 %RBP_6, -32
%218 = inttoptr i64 %217 to i32*
store i32 -1, i32* %218, align 1
%RIP_47 = 4195628
%EIP_37 = 4195628
store i32 4195647, i32* %EIP
store i64 %RBP_6, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_40052C:                                        ; preds = %bb_40050C
%RIP_64 = 4195635
%EIP_50 = 4195635
%RBP_10 = %RSP-8
%219 = add i64 %RBP_10, -32
%220 = inttoptr i64 %219 to i32*
store i32 1, i32* %220, align 1
%RIP_65 = 4195640
%EIP_51 = 4195640
store i32 4195647, i32* %EIP
store i64 %RBP_10, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_400538:                                        ; preds = %bb_40051B
%RIP_68 = 4195647
%EIP_53 = 4195647
%RBP_11 = %RSP-8
%221 = add i64 %RBP_11, -32
%222 = inttoptr i64 %221 to i32*
store i32 0, i32* %222, align 1
store i32 %EIP_53, i32* %EIP
store i64 %RBP_11, i64* %RBP
store i64 %RIP_68, i64* %RIP
br label %bb_40053F
bb_40053F:                                        ; preds = %bb_400538, %bb_40052C, %bb_400520
%RIP_56 = 4195650
%EIP_44 = 4195650
%RBP_8 = %RSP-8
%223 = add i64 %RBP_8, -32
%224 = inttoptr i64 %223 to i32*
%EAX_3 = %RSP-40
%RAX_5 = %RSP-45
%RAX_6 = %RSP-40
%225 = lshr i32 %EAX_3, 8
%RIP_57 = 4195651
%EIP_45 = 4195651
%RSP_2 = %RSP-8
%RSP_3 = %RSP
%ESP_1 = %RSP
%226 = sub i64 %RSP_3, 8
%227 = inttoptr i64 %226 to i64*
%RBP_9 = %RSP-8
%EBP_1 = %RSP-8
%RIP_58 = 4195652
%EIP_46 = 4195652
%RSP_4 = %RSP+8
%228 = inttoptr i64 %RSP_3 to i64*
%RIP_59 = %RSP
%ESP_2 = %RSP+8
%EIP_47 = %RSP
store i32 %EAX_3, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %EIP_47, i32* %EIP
store i32 %ESP_2, i32* %ESP
store i64 %RAX_6, i64* %RAX
store i64 %RBP_9, i64* %RBP
store i64 %RIP_59, i64* %RIP
store i64 %RSP_4, i64* %RSP
br label %exit_fn_400480
}

