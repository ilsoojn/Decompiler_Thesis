define void @fn_400550(%regset* noalias nocapture) {
entry_fn_400550:
  %RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
  %RIP_init = load i64, i64* %RIP_ptr
  %RIP = alloca i64
  store i64 %RIP_init, i64* %RIP
  %EIP_init = trunc i64 %RIP_init to i32
  %EIP = alloca i32
  store i32 %EIP_init, i32* %EIP
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
  %EBP_init = trunc i64 %RBP_init to i32
  %EBP = alloca i32
  store i32 %EBP_init, i32* %EBP
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
  %RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
  %RAX_init = load i64, i64* %RAX_ptr
  %RAX = alloca i64
  store i64 %RAX_init, i64* %RAX
  %EAX_init = trunc i64 %RAX_init to i32
  %EAX = alloca i32
  store i32 %EAX_init, i32* %EAX
  %5 = lshr i64 %RAX_init, 8
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
  %ZMM2_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 87
  %ZMM2_init = load <16 x float>, <16 x float>* %ZMM2_ptr
  %ZMM2 = alloca <16 x float>
  store <16 x float> %ZMM2_init, <16 x float>* %ZMM2
  %10 = bitcast <16 x float> %ZMM2_init to i512
  %11 = trunc i512 %10 to i128
  %XMM2_init = bitcast i128 %11 to <4 x float>
  %XMM2 = alloca <4 x float>
  store <4 x float> %XMM2_init, <4 x float>* %XMM2
  %12 = bitcast <16 x float> %ZMM2_init to i512
  %13 = trunc i512 %12 to i256
  %YMM2_init = bitcast i256 %13 to <8 x float>
  %YMM2 = alloca <8 x float>
  store <8 x float> %YMM2_init, <8 x float>* %YMM2
  %RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
  %RDI_init = load i64, i64* %RDI_ptr
  %RDI = alloca i64
  store i64 %RDI_init, i64* %RDI
  %EDI_init = trunc i64 %RDI_init to i32
  %EDI = alloca i32
  store i32 %EDI_init, i32* %EDI
  br label %bb_400550
exit_fn_400550:                                   ; preds = %bb_4006BC
  %16 = load i64, i64* %RAX
  store i64 %16, i64* %RAX_ptr
  %17 = load i64, i64* %RBP
  store i64 %17, i64* %RBP_ptr
  %18 = load i64, i64* %RDI
  store i64 %18, i64* %RDI_ptr
  %19 = load i64, i64* %RIP
  store i64 %19, i64* %RIP_ptr
  %20 = load i64, i64* %RSP
  store i64 %20, i64* %RSP_ptr
  %21 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %21, <16 x float>* %ZMM0_ptr
  %22 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %22, <16 x float>* %ZMM1_ptr
  %23 = load <16 x float>, <16 x float>* %ZMM2
  store <16 x float> %23, <16 x float>* %ZMM2_ptr
  ret void
bb_400550:                                        ; preds = %entry_fn_400550
  %RIP_1 = 1+4195664
  %EIP_0 = trunc i64 %RIP_1 to i32
  %RBP_0 = load i64, i64* %RBP
  %RSP_0 = load i64, i64* %RSP
  %25 = %RSP_0-8
  store i64 %RBP_0, i64* %25, align 1
  %RSP_1 = %RSP_0-8
  %ESP_0 = trunc i64 %RSP_1 to i32
  %RIP_2 = %RIP_1+3
  %EIP_1 = trunc i64 %RIP_2 to i32
  %EBP_0 = trunc i64 %RSP_1 to i32
  %RIP_3 = %RIP_2+4
  %EIP_2 = trunc i64 %RIP_3 to i32
  %RSP_2 = %RSP_1-80
  %ESP_1 = trunc i64 %RSP_2 to i32
  %RIP_4 = %RIP_3+3
  %EIP_3 = trunc i64 %RIP_4 to i32
  %ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
  %26 = bitcast <16 x float> %ZMM0_0 to i512
  %XMM0_0 = trunc i512 %26 to i128
  %27 = bitcast i128 %XMM0_0 to <4 x float>
  %28 = bitcast <4 x float> %27 to <2 x i64>
  %29 = bitcast i128 %XMM0_0 to <4 x float>
  %30 = bitcast <4 x float> %29 to <2 x i64>
  %31 = xor <2 x i64> %28, %30
  %XMM0_1 = bitcast <2 x i64> %31 to i128
  %32 = bitcast <16 x float> %ZMM0_0 to i512
  %YMM0_0 = trunc i512 %32 to i256
  %YMM0_1 = %YMM0_0 : %XMM0_1
  %35 = bitcast <16 x float> %ZMM0_0 to i512
  %ZMM0_1 = %35 : %XMM0_1
  %RIP_5 = %RIP_4+10
  %EIP_4 = trunc i64 %RIP_5 to i32
  %RIP_6 = %RIP_5+5
  %EIP_5 = trunc i64 %RIP_6 to i32
  %38 = sitofp i64 4003244811 to double
  %39 = bitcast double %38 to i64
  %ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
  %40 = bitcast <16 x float> %ZMM1_0 to i512
  %XMM1_0 = trunc i512 %40 to i128
  %XMM1_1 = %XMM1_0 : %39
  %43 = bitcast <16 x float> %ZMM1_0 to i512
  %YMM1_0 = trunc i512 %43 to i256
  %YMM1_1 = %YMM1_0 : %XMM1_1
  %46 = bitcast <16 x float> %ZMM1_0 to i512
  %ZMM1_1 = %46 : %XMM1_1
  %RIP_7 = %RIP_6+10
  %EIP_6 = trunc i64 %RIP_7 to i32
  %RIP_8 = %RIP_7+5
  %EIP_7 = trunc i64 %RIP_8 to i32
  %49 = sitofp i64 1336630430 to double
  %50 = bitcast double %49 to i64
  %ZMM2_0 = load <16 x float>, <16 x float>* %ZMM2
  %51 = bitcast <16 x float> %ZMM2_0 to i512
  %XMM2_0 = trunc i512 %51 to i128
  %XMM2_1 = %XMM2_0 : %50
  %54 = bitcast <16 x float> %ZMM2_0 to i512
  %YMM2_0 = trunc i512 %54 to i256
  %YMM2_1 = %YMM2_0 : %XMM2_1
  %57 = bitcast <16 x float> %ZMM2_0 to i512
  %ZMM2_1 = %57 : %XMM2_1
  %RIP_9 = %RIP_8+7
  %EIP_8 = trunc i64 %RIP_9 to i32
  %61 = %RSP_1-4
  store i32 0, i32* %61, align 1
  %RIP_10 = %RIP_9+5
  %EIP_9 = trunc i64 %RIP_10 to i32
  %62 = trunc i128 %XMM2_1 to i64
  %63 = bitcast i64 %62 to double
  %65 = %RSP_1-24
  store double %63, double* %65, align 1
  %RIP_11 = %RIP_10+5
  %EIP_10 = trunc i64 %RIP_11 to i32
  %66 = trunc i128 %XMM1_1 to i64
  %67 = bitcast i64 %66 to double
  %69 = %RSP_1-16
  store double %67, double* %69, align 1
  %RIP_12 = %RIP_11+5
  %EIP_11 = trunc i64 %RIP_12 to i32
  %71 = %RSP_1-16
  %72 = load double, double* %71, align 1
  %73 = bitcast double %72 to i64
  %XMM1_2 = %XMM1_1 : %73
  %YMM1_2 = %YMM1_1 : %XMM1_2
  %ZMM1_2 = %ZMM1_1 : %XMM1_2
  %RIP_13 = %RIP_12+5
  %EIP_12 = trunc i64 %RIP_13 to i32
  %80 = trunc i128 %XMM1_2 to i64
  %81 = bitcast i64 %80 to double
  %83 = %RSP_1-24
  %84 = load double, double* %83, align 1
  %85 = fadd double %81, %84
  %86 = bitcast double %85 to i64
  %XMM1_3 = %XMM1_2 : %86
  %YMM1_3 = %YMM1_2 : %XMM1_3
  %ZMM1_3 = %ZMM1_2 : %XMM1_3
  %RIP_14 = %RIP_13+5
  %EIP_13 = trunc i64 %RIP_14 to i32
  %93 = trunc i128 %XMM1_3 to i64
  %94 = bitcast i64 %93 to double
  %96 = %RSP_1-32
  store double %94, double* %96, align 1
  %RIP_15 = %RIP_14+5
  %EIP_14 = trunc i64 %RIP_15 to i32
  %98 = %RSP_1-16
  %99 = load double, double* %98, align 1
  %100 = bitcast double %99 to i64
  %XMM1_4 = %XMM1_3 : %100
  %YMM1_4 = %YMM1_3 : %XMM1_4
  %ZMM1_4 = %ZMM1_3 : %XMM1_4
  %RIP_16 = %RIP_15+5
  %EIP_15 = trunc i64 %RIP_16 to i32
  %107 = trunc i128 %XMM1_4 to i64
  %108 = bitcast i64 %107 to double
  %110 = %RSP_1-24
  %111 = load double, double* %110, align 1
  %112 = fsub double %108, %111
  %113 = bitcast double %112 to i64
  %XMM1_5 = %XMM1_4 : %113
  %YMM1_5 = %YMM1_4 : %XMM1_5
  %ZMM1_5 = %ZMM1_4 : %XMM1_5
  %RIP_17 = %RIP_16+5
  %EIP_16 = trunc i64 %RIP_17 to i32
  %120 = trunc i128 %XMM1_5 to i64
  %121 = bitcast i64 %120 to double
  %123 = %RSP_1-40
  store double %121, double* %123, align 1
  %RIP_18 = %RIP_17+5
  %EIP_17 = trunc i64 %RIP_18 to i32
  %125 = %RSP_1-16
  %126 = load double, double* %125, align 1
  %127 = bitcast double %126 to i64
  %XMM1_6 = %XMM1_5 : %127
  %YMM1_6 = %YMM1_5 : %XMM1_6
  %ZMM1_6 = %ZMM1_5 : %XMM1_6
  %RIP_19 = %RIP_18+5
  %EIP_18 = trunc i64 %RIP_19 to i32
  %134 = trunc i128 %XMM1_6 to i64
  %135 = bitcast i64 %134 to double
  %137 = %RSP_1-24
  %138 = load double, double* %137, align 1
  %139 = fmul double %135, %138
  %140 = bitcast double %139 to i64
  %XMM1_7 = %XMM1_6 : %140
  %YMM1_7 = %YMM1_6 : %XMM1_7
  %ZMM1_7 = %ZMM1_6 : %XMM1_7
  %RIP_20 = %RIP_19+5
  %EIP_19 = trunc i64 %RIP_20 to i32
  %147 = trunc i128 %XMM1_7 to i64
  %148 = bitcast i64 %147 to double
  %150 = %RSP_1-48
  store double %148, double* %150, align 1
  %RIP_21 = %RIP_20+5
  %EIP_20 = trunc i64 %RIP_21 to i32
  %152 = %RSP_1-16
  %153 = load double, double* %152, align 1
  %154 = bitcast double %153 to i64
  %XMM1_8 = %XMM1_7 : %154
  %YMM1_8 = %YMM1_7 : %XMM1_8
  %ZMM1_8 = %ZMM1_7 : %XMM1_8
  %RIP_22 = %RIP_21+5
  %EIP_21 = trunc i64 %RIP_22 to i32
  %161 = trunc i128 %XMM1_8 to i64
  %162 = bitcast i64 %161 to double
  %164 = %RSP_1-24
  %165 = load double, double* %164, align 1
  %166 = fdiv double %162, %165
  %167 = bitcast double %166 to i64
  %XMM1_9 = %XMM1_8 : %167
  %YMM1_9 = %YMM1_8 : %XMM1_9
  %ZMM1_9 = %ZMM1_8 : %XMM1_9
  %RIP_23 = %RIP_22+5
  %EIP_22 = trunc i64 %RIP_23 to i32
  %174 = trunc i128 %XMM1_9 to i64
  %175 = bitcast i64 %174 to double
  %177 = %RSP_1-56
  store double %175, double* %177, align 1
  %RIP_24 = %RIP_23+5
  %EIP_23 = trunc i64 %RIP_24 to i32
  %179 = %RSP_1-32
  %180 = load double, double* %179, align 1
  %181 = bitcast double %180 to i64
  %XMM1_10 = %XMM1_9 : %181
  %YMM1_10 = %YMM1_9 : %XMM1_10
  %ZMM1_10 = %ZMM1_9 : %XMM1_10
  %RIP_25 = %RIP_24+5
  %EIP_24 = trunc i64 %RIP_25 to i32
  %188 = trunc i128 %XMM1_10 to i64
  %189 = bitcast i64 %188 to double
  %191 = %RSP_1-40
  %192 = load double, double* %191, align 1
  %193 = fmul double %189, %192
  %194 = bitcast double %193 to i64
  %XMM1_11 = %XMM1_10 : %194
  %YMM1_11 = %YMM1_10 : %XMM1_11
  %ZMM1_11 = %ZMM1_10 : %XMM1_11
  %RIP_26 = %RIP_25+5
  %EIP_25 = trunc i64 %RIP_26 to i32
  %201 = trunc i128 %XMM1_11 to i64
  %202 = bitcast i64 %201 to double
  %204 = %RSP_1-48
  %205 = load double, double* %204, align 1
  %206 = fdiv double %202, %205
  %207 = bitcast double %206 to i64
  %XMM1_12 = %XMM1_11 : %207
  %YMM1_12 = %YMM1_11 : %XMM1_12
  %ZMM1_12 = %ZMM1_11 : %XMM1_12
  %RIP_27 = %RIP_26+5
  %EIP_26 = trunc i64 %RIP_27 to i32
  %214 = trunc i128 %XMM1_12 to i64
  %215 = bitcast i64 %214 to double
  %217 = %RSP_1-56
  %218 = load double, double* %217, align 1
  %219 = fadd double %215, %218
  %220 = bitcast double %219 to i64
  %XMM1_13 = %XMM1_12 : %220
  %YMM1_13 = %YMM1_12 : %XMM1_13
  %ZMM1_13 = %ZMM1_12 : %XMM1_13
  %RIP_28 = %RIP_27+5
  %EIP_27 = trunc i64 %RIP_28 to i32
  %227 = trunc i128 %XMM1_13 to i64
  %228 = bitcast i64 %227 to double
  %230 = %RSP_1-64
  store double %228, double* %230, align 1
  %RIP_29 = %RIP_28+5
  %EIP_28 = trunc i64 %RIP_29 to i32
  %231 = trunc i128 %XMM0_1 to i64
  %232 = bitcast i64 %231 to double
  %234 = %RSP_1-64
  %235 = load double, double* %234, align 1
  %ZF_0 = fcmp ueq double %232, %235
  %PF_0 = fcmp uno double %232, %235
  %CF_0 = fcmp ult double %232, %235
  %236 = zext i1 %CF_0 to i32
  %237 = shl i32 %236, 0
  %239 = zext i1 %PF_0 to i32
  %240 = shl i32 %239, 2
  %242 = zext i1 false to i32
  %243 = shl i32 %242, 4
  %245 = zext i1 %ZF_0 to i32
  %246 = shl i32 %245, 6
  %248 = zext i1 false to i32
  %249 = shl i32 %248, 7
  %251 = zext i1 false to i32
  %252 = shl i32 %251, 11
  %RIP_30 = %RIP_29+6
  %EIP_29 = trunc i64 %RIP_30 to i32
  %CC_BE_0 = or i1 %CF_0, %ZF_0
  store i32 1336630430, i32* %EAX
  store i32 %EBP_0, i32* %EBP
  store i32 4195831, i32* %EIP
  store i32 %ESP_1, i32* %ESP
  store i64 1336630430, i64* %RAX
  store i64 %RSP_1, i64* %RBP
  store i64 4195831, i64* %RIP
  store i64 %RSP_2, i64* %RSP
  %253 = bitcast i128 %XMM0_1 to <4 x float>
  store <4 x float> %253, <4 x float>* %XMM0
  %254 = bitcast i128 %XMM1_13 to <4 x float>
  store <4 x float> %254, <4 x float>* %XMM1
  %255 = bitcast i128 %XMM2_1 to <4 x float>
  store <4 x float> %255, <4 x float>* %XMM2
  %256 = bitcast i256 %YMM0_1 to <8 x float>
  store <8 x float> %256, <8 x float>* %YMM0
  %257 = bitcast i256 %YMM1_13 to <8 x float>
  store <8 x float> %257, <8 x float>* %YMM1
  %258 = bitcast i256 %YMM2_1 to <8 x float>
  store <8 x float> %258, <8 x float>* %YMM2
  %259 = bitcast i512 %ZMM0_1 to <16 x float>
  store <16 x float> %259, <16 x float>* %ZMM0
  %260 = bitcast i512 %ZMM1_13 to <16 x float>
  store <16 x float> %260, <16 x float>* %ZMM1
  %261 = bitcast i512 %ZMM2_1 to <16 x float>
  store <16 x float> %261, <16 x float>* %ZMM2
  br i1 %CC_BE_0, label %bb_4005F7, label %bb_4005EA
bb_4005EA:                                        ; preds = %bb_400550
  %RIP_33 = 4195818+5
  %EIP_31 = trunc i64 %RIP_33 to i32
  %RBP_1 = load i64, i64* %RBP
  %263 = %RBP_1-32
  %264 = load double, double* %263, align 1
  %EAX_2 = fptosi double %264 to i32
  %RAX_2 = load i64, i64* %RAX
  %RAX_3 = zext i32 %EAX_2 to i64
  %265 = lshr i32 %EAX_2, 8
  %RIP_34 = %RIP_33+3
  %EIP_32 = trunc i64 %RIP_34 to i32
  %267 = %RBP_1-4
  store i32 %EAX_2, i32* %267, align 1
  %RIP_35 = %RIP_34+5
  %EIP_33 = trunc i64 %RIP_35 to i32
  store i32 %EAX_2, i32* %EAX
  store i32 4196028, i32* %EIP
  store i64 %RAX_3, i64* %RAX
  store i64 %RBP_1, i64* %RBP
  store i64 4196028, i64* %RIP
  br label %bb_4006BC
bb_4005F7:                                        ; preds = %bb_400550
  %RIP_38 = 3+4195831
  %EIP_35 = trunc i64 %RIP_38 to i32
  %ZMM0_2 = load <16 x float>, <16 x float>* %ZMM0
  %268 = bitcast <16 x float> %ZMM0_2 to i512
  %XMM0_2 = trunc i512 %268 to i128
  %269 = bitcast i128 %XMM0_2 to <4 x float>
  %270 = bitcast <4 x float> %269 to <2 x i64>
  %271 = bitcast i128 %XMM0_2 to <4 x float>
  %272 = bitcast <4 x float> %271 to <2 x i64>
  %273 = xor <2 x i64> %270, %272
  %XMM0_3 = bitcast <2 x i64> %273 to i128
  %274 = bitcast <16 x float> %ZMM0_2 to i512
  %YMM0_2 = trunc i512 %274 to i256
  %YMM0_3 = %YMM0_2 : %XMM0_3
  %277 = bitcast <16 x float> %ZMM0_2 to i512
  %ZMM0_3 = %277 : %XMM0_3
  %RIP_39 = %RIP_38+5
  %EIP_36 = trunc i64 %RIP_39 to i32
  %RBP_2 = load i64, i64* %RBP
  %281 = %RBP_2-64
  %282 = load double, double* %281, align 1
  %283 = bitcast double %282 to i64
  %ZMM1_14 = load <16 x float>, <16 x float>* %ZMM1
  %284 = bitcast <16 x float> %ZMM1_14 to i512
  %XMM1_14 = trunc i512 %284 to i128
  %XMM1_15 = %XMM1_14 : %283
  %287 = bitcast <16 x float> %ZMM1_14 to i512
  %YMM1_14 = trunc i512 %287 to i256
  %YMM1_15 = %YMM1_14 : %XMM1_15
  %290 = bitcast <16 x float> %ZMM1_14 to i512
  %ZMM1_15 = %290 : %XMM1_15
  %RIP_40 = %RIP_39+4
  %EIP_37 = trunc i64 %RIP_40 to i32
  %293 = trunc i128 %XMM1_15 to i64
  %294 = bitcast i64 %293 to double
  %295 = trunc i128 %XMM0_3 to i64
  %296 = bitcast i64 %295 to double
  %ZF_01 = fcmp ueq double %294, %296
  %PF_02 = fcmp uno double %294, %296
  %CF_03 = fcmp ult double %294, %296
  %297 = zext i1 %CF_03 to i32
  %298 = shl i32 %297, 0
  %300 = zext i1 %PF_02 to i32
  %301 = shl i32 %300, 2
  %303 = zext i1 false to i32
  %304 = shl i32 %303, 4
  %306 = zext i1 %ZF_01 to i32
  %307 = shl i32 %306, 6
  %309 = zext i1 false to i32
  %310 = shl i32 %309, 7
  %312 = zext i1 false to i32
  %313 = shl i32 %312, 11
  %RIP_41 = %RIP_40+6
  %EIP_38 = trunc i64 %RIP_41 to i32
  store i32 4195888, i32* %EIP
  store i64 %RBP_2, i64* %RBP
  store i64 4195888, i64* %RIP
  %314 = bitcast i128 %XMM0_3 to <4 x float>
  store <4 x float> %314, <4 x float>* %XMM0
  %315 = bitcast i128 %XMM1_15 to <4 x float>
  store <4 x float> %315, <4 x float>* %XMM1
  %316 = bitcast i256 %YMM0_3 to <8 x float>
  store <8 x float> %316, <8 x float>* %YMM0
  %317 = bitcast i256 %YMM1_15 to <8 x float>
  store <8 x float> %317, <8 x float>* %YMM1
  %318 = bitcast i512 %ZMM0_3 to <16 x float>
  store <16 x float> %318, <16 x float>* %ZMM0
  %319 = bitcast i512 %ZMM1_15 to <16 x float>
  store <16 x float> %319, <16 x float>* %ZMM1
  br i1 %CF_03, label %bb_400630, label %bb_400609
bb_400609:                                        ; preds = %bb_4005F7
  %RIP_50 = 10+4195849
  %EIP_45 = trunc i64 %RIP_50 to i32
  %RIP_51 = %RIP_50+5
  %EIP_46 = trunc i64 %RIP_51 to i32
  %320 = sitofp i64 5 to double
  %321 = bitcast double %320 to i64
  %ZMM0_4 = load <16 x float>, <16 x float>* %ZMM0
  %322 = bitcast <16 x float> %ZMM0_4 to i512
  %XMM0_4 = trunc i512 %322 to i128
  %XMM0_5 = %XMM0_4 : %321
  %325 = bitcast <16 x float> %ZMM0_4 to i512
  %YMM0_4 = trunc i512 %325 to i256
  %YMM0_5 = %YMM0_4 : %XMM0_5
  %328 = bitcast <16 x float> %ZMM0_4 to i512
  %ZMM0_5 = %328 : %XMM0_5
  %RIP_52 = %RIP_51+5
  %EIP_47 = trunc i64 %RIP_52 to i32
  %331 = trunc i128 %XMM0_5 to i64
  %332 = bitcast i64 %331 to double
  %RBP_5 = load i64, i64* %RBP
  %334 = %RBP_5-64
  %335 = load double, double* %334, align 1
  %ZF_07 = fcmp ueq double %332, %335
  %PF_08 = fcmp uno double %332, %335
  %CF_09 = fcmp ult double %332, %335
  %336 = zext i1 %CF_09 to i32
  %337 = shl i32 %336, 0
  %339 = zext i1 %PF_08 to i32
  %340 = shl i32 %339, 2
  %342 = zext i1 false to i32
  %343 = shl i32 %342, 4
  %345 = zext i1 %ZF_07 to i32
  %346 = shl i32 %345, 6
  %348 = zext i1 false to i32
  %349 = shl i32 %348, 7
  %351 = zext i1 false to i32
  %352 = shl i32 %351, 11
  %RIP_53 = %RIP_52+6
  %EIP_48 = trunc i64 %RIP_53 to i32
  %CC_BE_010 = or i1 %CF_09, %ZF_07
  store i32 5, i32* %EAX
  store i32 4195888, i32* %EIP
  store i64 5, i64* %RAX
  store i64 %RBP_5, i64* %RBP
  store i64 4195888, i64* %RIP
  %353 = bitcast i128 %XMM0_5 to <4 x float>
  store <4 x float> %353, <4 x float>* %XMM0
  %354 = bitcast i256 %YMM0_5 to <8 x float>
  store <8 x float> %354, <8 x float>* %YMM0
  %355 = bitcast i512 %ZMM0_5 to <16 x float>
  store <16 x float> %355, <16 x float>* %ZMM0
  br i1 %CC_BE_010, label %bb_400630, label %bb_400623
bb_400623:                                        ; preds = %bb_400609
  %RIP_63 = 4195875+5
  %EIP_56 = trunc i64 %RIP_63 to i32
  %RBP_7 = load i64, i64* %RBP
  %357 = %RBP_7-40
  %358 = load double, double* %357, align 1
  %EAX_6 = fptosi double %358 to i32
  %RAX_8 = load i64, i64* %RAX
  %RAX_9 = zext i32 %EAX_6 to i64
  %359 = lshr i32 %EAX_6, 8
  %RIP_64 = %RIP_63+3
  %EIP_57 = trunc i64 %RIP_64 to i32
  %361 = %RBP_7-4
  store i32 %EAX_6, i32* %361, align 1
  %RIP_65 = %RIP_64+5
  %EIP_58 = trunc i64 %RIP_65 to i32
  store i32 %EAX_6, i32* %EAX
  store i32 4196028, i32* %EIP
  store i64 %RAX_9, i64* %RAX
  store i64 %RBP_7, i64* %RBP
  store i64 4196028, i64* %RIP
  br label %bb_4006BC
bb_400630:                                        ; preds = %bb_400609, %bb_4005F7
  %RIP_56 = 10+4195888
  %EIP_50 = trunc i64 %RIP_56 to i32
  %RIP_57 = %RIP_56+5
  %EIP_51 = trunc i64 %RIP_57 to i32
  %362 = sitofp i64 5 to double
  %363 = bitcast double %362 to i64
  %ZMM0_6 = load <16 x float>, <16 x float>* %ZMM0
  %364 = bitcast <16 x float> %ZMM0_6 to i512
  %XMM0_6 = trunc i512 %364 to i128
  %XMM0_7 = %XMM0_6 : %363
  %367 = bitcast <16 x float> %ZMM0_6 to i512
  %YMM0_6 = trunc i512 %367 to i256
  %YMM0_7 = %YMM0_6 : %XMM0_7
  %370 = bitcast <16 x float> %ZMM0_6 to i512
  %ZMM0_7 = %370 : %XMM0_7
  %RIP_58 = %RIP_57+5
  %EIP_52 = trunc i64 %RIP_58 to i32
  %RBP_6 = load i64, i64* %RBP
  %374 = %RBP_6-64
  %375 = load double, double* %374, align 1
  %376 = bitcast double %375 to i64
  %ZMM1_16 = load <16 x float>, <16 x float>* %ZMM1
  %377 = bitcast <16 x float> %ZMM1_16 to i512
  %XMM1_16 = trunc i512 %377 to i128
  %XMM1_17 = %XMM1_16 : %376
  %380 = bitcast <16 x float> %ZMM1_16 to i512
  %YMM1_16 = trunc i512 %380 to i256
  %YMM1_17 = %YMM1_16 : %XMM1_17
  %383 = bitcast <16 x float> %ZMM1_16 to i512
  %ZMM1_17 = %383 : %XMM1_17
  %RIP_59 = %RIP_58+4
  %EIP_53 = trunc i64 %RIP_59 to i32
  %386 = trunc i128 %XMM1_17 to i64
  %387 = bitcast i64 %386 to double
  %388 = trunc i128 %XMM0_7 to i64
  %389 = bitcast i64 %388 to double
  %ZF_011 = fcmp ueq double %387, %389
  %PF_012 = fcmp uno double %387, %389
  %CF_013 = fcmp ult double %387, %389
  %390 = zext i1 %CF_013 to i32
  %391 = shl i32 %390, 0
  %393 = zext i1 %PF_012 to i32
  %394 = shl i32 %393, 2
  %396 = zext i1 false to i32
  %397 = shl i32 %396, 4
  %399 = zext i1 %ZF_011 to i32
  %400 = shl i32 %399, 6
  %402 = zext i1 false to i32
  %403 = shl i32 %402, 7
  %405 = zext i1 false to i32
  %406 = shl i32 %405, 11
  %RIP_60 = %RIP_59+6
  %EIP_54 = trunc i64 %RIP_60 to i32
  %CC_NE_0 = xor i1 %ZF_011, true
  store i32 5, i32* %EAX
  store i32 4195937, i32* %EIP
  store i64 5, i64* %RAX
  store i64 %RBP_6, i64* %RBP
  store i64 4195937, i64* %RIP
  %407 = bitcast i128 %XMM0_7 to <4 x float>
  store <4 x float> %407, <4 x float>* %XMM0
  %408 = bitcast i128 %XMM1_17 to <4 x float>
  store <4 x float> %408, <4 x float>* %XMM1
  %409 = bitcast i256 %YMM0_7 to <8 x float>
  store <8 x float> %409, <8 x float>* %YMM0
  %410 = bitcast i256 %YMM1_17 to <8 x float>
  store <8 x float> %410, <8 x float>* %YMM1
  %411 = bitcast i512 %ZMM0_7 to <16 x float>
  store <16 x float> %411, <16 x float>* %ZMM0
  %412 = bitcast i512 %ZMM1_17 to <16 x float>
  store <16 x float> %412, <16 x float>* %ZMM1
  br i1 %CC_NE_0, label %bb_400661, label %bb_40064E
bb_40064E:                                        ; preds = %bb_400630
  %RIP_68 = 4195918+6
  %EIP_60 = trunc i64 %RIP_68 to i32
  store i32 4195937, i32* %EIP
  store i64 4195937, i64* %RIP
bb_400654:                                        ; preds = %bb_40064E
  %RIP_74 = 4195924+5
  %EIP_64 = trunc i64 %RIP_74 to i32
  %RBP_8 = load i64, i64* %RBP
  %415 = %RBP_8-48
  %416 = load double, double* %415, align 1
  %EAX_7 = fptosi double %416 to i32
  %RAX_10 = load i64, i64* %RAX
  %RAX_11 = zext i32 %EAX_7 to i64
  %417 = lshr i32 %EAX_7, 8
  %RIP_75 = %RIP_74+3
  %EIP_65 = trunc i64 %RIP_75 to i32
  %419 = %RBP_8-4
  store i32 %EAX_7, i32* %419, align 1
  %RIP_76 = %RIP_75+5
  %EIP_66 = trunc i64 %RIP_76 to i32
  store i32 %EAX_7, i32* %EAX
  store i32 4196028, i32* %EIP
  store i64 %RAX_11, i64* %RAX
  store i64 %RBP_8, i64* %RBP
  store i64 4196028, i64* %RIP
  br label %bb_4006BC
bb_400661:                                        ; preds = %bb_40064E, %bb_400630
  %RIP_71 = 4195937+5
  %EIP_62 = trunc i64 %RIP_71 to i32
  store i32 4195942, i32* %EIP
  store i64 4195942, i64* %RIP
  br label %bb_400666
bb_400666:                                        ; preds = %bb_400678, %bb_400661
  %RIP_79 = 3+4195942
  %EIP_68 = trunc i64 %RIP_79 to i32
  %ZMM0_8 = load <16 x float>, <16 x float>* %ZMM0
  %420 = bitcast <16 x float> %ZMM0_8 to i512
  %XMM0_8 = trunc i512 %420 to i128
  %421 = bitcast i128 %XMM0_8 to <4 x float>
  %422 = bitcast <4 x float> %421 to <2 x i64>
  %423 = bitcast i128 %XMM0_8 to <4 x float>
  %424 = bitcast <4 x float> %423 to <2 x i64>
  %425 = xor <2 x i64> %422, %424
  %XMM0_9 = bitcast <2 x i64> %425 to i128
  %426 = bitcast <16 x float> %ZMM0_8 to i512
  %YMM0_8 = trunc i512 %426 to i256
  %YMM0_9 = %YMM0_8 : %XMM0_9
  %429 = bitcast <16 x float> %ZMM0_8 to i512
  %ZMM0_9 = %429 : %XMM0_9
  %RIP_80 = %RIP_79+5
  %EIP_69 = trunc i64 %RIP_80 to i32
  %RBP_9 = load i64, i64* %RBP
  %433 = %RBP_9-64
  %434 = load double, double* %433, align 1
  %435 = bitcast double %434 to i64
  %ZMM1_18 = load <16 x float>, <16 x float>* %ZMM1
  %436 = bitcast <16 x float> %ZMM1_18 to i512
  %XMM1_18 = trunc i512 %436 to i128
  %XMM1_19 = %XMM1_18 : %435
  %439 = bitcast <16 x float> %ZMM1_18 to i512
  %YMM1_18 = trunc i512 %439 to i256
  %YMM1_19 = %YMM1_18 : %XMM1_19
  %442 = bitcast <16 x float> %ZMM1_18 to i512
  %ZMM1_19 = %442 : %XMM1_19
  %RIP_81 = %RIP_80+4
  %EIP_70 = trunc i64 %RIP_81 to i32
  %445 = trunc i128 %XMM1_19 to i64
  %446 = bitcast i64 %445 to double
  %447 = trunc i128 %XMM0_9 to i64
  %448 = bitcast i64 %447 to double
  %ZF_015 = fcmp ueq double %446, %448
  %PF_016 = fcmp uno double %446, %448
  %CF_017 = fcmp ult double %446, %448
  %449 = zext i1 %CF_017 to i32
  %450 = shl i32 %449, 0
  %452 = zext i1 %PF_016 to i32
  %453 = shl i32 %452, 2
  %455 = zext i1 false to i32
  %456 = shl i32 %455, 4
  %458 = zext i1 %ZF_015 to i32
  %459 = shl i32 %458, 6
  %461 = zext i1 false to i32
  %462 = shl i32 %461, 7
  %464 = zext i1 false to i32
  %465 = shl i32 %464, 11
  %RIP_82 = %RIP_81+6
  %EIP_71 = trunc i64 %RIP_82 to i32
  %CC_BE_018 = or i1 %CF_017, %ZF_015
  store i32 4196011, i32* %EIP
  store i64 %RBP_9, i64* %RBP
  store i64 4196011, i64* %RIP
  %466 = bitcast i128 %XMM0_9 to <4 x float>
  store <4 x float> %466, <4 x float>* %XMM0
  %467 = bitcast i128 %XMM1_19 to <4 x float>
  store <4 x float> %467, <4 x float>* %XMM1
  %468 = bitcast i256 %YMM0_9 to <8 x float>
  store <8 x float> %468, <8 x float>* %YMM0
  %469 = bitcast i256 %YMM1_19 to <8 x float>
  store <8 x float> %469, <8 x float>* %YMM1
  %470 = bitcast i512 %ZMM0_9 to <16 x float>
  store <16 x float> %470, <16 x float>* %ZMM0
  %471 = bitcast i512 %ZMM1_19 to <16 x float>
  store <16 x float> %471, <16 x float>* %ZMM1
  br i1 %CC_BE_018, label %bb_4006AB, label %bb_400678
bb_400678:                                        ; preds = %bb_400666
  %RIP_85 = 4195960+8
  %EIP_73 = trunc i64 %RIP_85 to i32
  %RIP_86 = %RIP_85+5
  %EIP_74 = trunc i64 %RIP_86 to i32
  %RBP_10 = load i64, i64* %RBP
  %473 = %RBP_10-64
  %474 = load double, double* %473, align 1
  %475 = bitcast double %474 to i64
  %ZMM0_10 = load <16 x float>, <16 x float>* %ZMM0
  %476 = bitcast <16 x float> %ZMM0_10 to i512
  %XMM0_10 = trunc i512 %476 to i128
  %XMM0_11 = %XMM0_10 : %475
  %479 = bitcast <16 x float> %ZMM0_10 to i512
  %YMM0_10 = trunc i512 %479 to i256
  %YMM0_11 = %YMM0_10 : %XMM0_11
  %482 = bitcast <16 x float> %ZMM0_10 to i512
  %ZMM0_11 = %482 : %XMM0_11
  %RIP_87 = %RIP_86+2
  %EIP_75 = trunc i64 %RIP_87 to i32
  %RAX_12 = load i64, i64* %RAX
  %EAX_8 = trunc i64 %RAX_12 to i32
  %486 = and i32 %EAX_8, -256
  %EAX_9 = or i32 1, %486
  %487 = and i64 %RAX_12, -256
  %RAX_13 = or i64 1, %487
  %RIP_88 = %RIP_87+5
  %EIP_76 = trunc i64 %RIP_88 to i32
  %RSP_7 = load i64, i64* %RSP
  %488 = %RSP_7-8
  store i64 4195980, i64* %488
  %ESP_5 = trunc i64 %RSP_8 to i32
  store i32 %EAX_9, i32* %EAX
  store i32 4196192, i32* %EDI
  store i32 %EIP_76, i32* %EIP
  store i32 %ESP_5, i32* %ESP
  store i64 %RAX_13, i64* %RAX
  store i64 %RBP_10, i64* %RBP
  store i64 4196192, i64* %RDI
  store i64 %RIP_88, i64* %RIP
  store i64 %RSP_8, i64* %RSP
  %489 = bitcast i128 %XMM0_11 to <4 x float>
  store <4 x float> %489, <4 x float>* %XMM0
  %490 = bitcast i256 %YMM0_11 to <8 x float>
  store <8 x float> %490, <8 x float>* %YMM0
  %491 = bitcast i512 %ZMM0_11 to <16 x float>
  store <16 x float> %491, <16 x float>* %ZMM0
  %494 = load i64, i64* %RAX
  store i64 %494, i64* %RAX_ptr
  %495 = load i64, i64* %RBP
  store i64 %495, i64* %RBP_ptr
  %496 = load i64, i64* %RDI
  store i64 %496, i64* %RDI_ptr
  %497 = load i64, i64* %RIP
  store i64 %497, i64* %RIP_ptr
  %498 = load i64, i64* %RSP
  store i64 %498, i64* %RSP_ptr
  %499 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %499, <16 x float>* %ZMM0_ptr
  %500 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %500, <16 x float>* %ZMM1_ptr
  %501 = load <16 x float>, <16 x float>* %ZMM2
  store <16 x float> %501, <16 x float>* %ZMM2_ptr
  call void @fn_400410(%regset* %0)
  %504 = load i64, i64* %RAX_ptr
  store i64 %504, i64* %RAX
  %505 = load i64, i64* %RBP_ptr
  store i64 %505, i64* %RBP
  %506 = load i64, i64* %RDI_ptr
  store i64 %506, i64* %RDI
  %507 = load i64, i64* %RIP_ptr
  store i64 %507, i64* %RIP
  %508 = load i64, i64* %RSP_ptr
  store i64 %508, i64* %RSP
  %509 = load <16 x float>, <16 x float>* %ZMM0_ptr
  store <16 x float> %509, <16 x float>* %ZMM0
  %510 = load <16 x float>, <16 x float>* %ZMM1_ptr
  store <16 x float> %510, <16 x float>* %ZMM1
  %511 = load <16 x float>, <16 x float>* %ZMM2_ptr
  store <16 x float> %511, <16 x float>* %ZMM2
  %RIP_89 = load i64, i64* %RIP
  %RIP_90 = %RIP_89+9
  %EIP_77 = trunc i64 %RIP_90 to i32
  %512 = inttoptr i64 4196184 to double*
  %513 = load double, double* %512, align 1
  %514 = bitcast double %513 to i64
  %ZMM0_12 = load <16 x float>, <16 x float>* %ZMM0
  %515 = bitcast <16 x float> %ZMM0_12 to i512
  %XMM0_12 = trunc i512 %515 to i128
  %XMM0_13 = %XMM0_12 : %514
  %518 = bitcast <16 x float> %ZMM0_12 to i512
  %YMM0_12 = trunc i512 %518 to i256
  %YMM0_13 = %YMM0_12 : %XMM0_13
  %521 = bitcast <16 x float> %ZMM0_12 to i512
  %ZMM0_13 = %521 : %XMM0_13
  %RIP_91 = %RIP_90+5
  %EIP_78 = trunc i64 %RIP_91 to i32
  %RBP_11 = load i64, i64* %RBP
  %525 = %RBP_11-64
  %526 = load double, double* %525, align 1
  %527 = bitcast double %526 to i64
  %ZMM1_20 = load <16 x float>, <16 x float>* %ZMM1
  %528 = bitcast <16 x float> %ZMM1_20 to i512
  %XMM1_20 = trunc i512 %528 to i128
  %XMM1_21 = %XMM1_20 : %527
  %531 = bitcast <16 x float> %ZMM1_20 to i512
  %YMM1_20 = trunc i512 %531 to i256
  %YMM1_21 = %YMM1_20 : %XMM1_21
  %534 = bitcast <16 x float> %ZMM1_20 to i512
  %ZMM1_21 = %534 : %XMM1_21
  %RIP_92 = %RIP_91+4
  %EIP_79 = trunc i64 %RIP_92 to i32
  %537 = trunc i128 %XMM1_21 to i64
  %538 = bitcast i64 %537 to double
  %539 = trunc i128 %XMM0_13 to i64
  %540 = bitcast i64 %539 to double
  %541 = fsub double %538, %540
  %542 = bitcast double %541 to i64
  %XMM1_22 = %XMM1_21 : %542
  %YMM1_22 = %YMM1_21 : %XMM1_22
  %ZMM1_22 = %ZMM1_21 : %XMM1_22
  %RIP_93 = %RIP_92+5
  %EIP_80 = trunc i64 %RIP_93 to i32
  %549 = trunc i128 %XMM1_22 to i64
  %550 = bitcast i64 %549 to double
  %552 = %RBP_11-64
  store double %550, double* %552, align 1
  %RIP_94 = %RIP_93+3
  %EIP_81 = trunc i64 %RIP_94 to i32
  %RAX_14 = load i64, i64* %RAX
  %EAX_10 = trunc i64 %RAX_14 to i32
  %554 = %RBP_11-68
  store i32 %EAX_10, i32* %554, align 1
  %RIP_95 = %RIP_94+5
  %EIP_82 = trunc i64 %RIP_95 to i32
  store i32 %EAX_10, i32* %EAX
  store i32 4195942, i32* %EIP
  store i64 %RAX_14, i64* %RAX
  store i64 %RBP_11, i64* %RBP
  store i64 4195942, i64* %RIP
  %555 = bitcast i128 %XMM0_13 to <4 x float>
  store <4 x float> %555, <4 x float>* %XMM0
  %556 = bitcast i128 %XMM1_22 to <4 x float>
  store <4 x float> %556, <4 x float>* %XMM1
  %557 = bitcast i256 %YMM0_13 to <8 x float>
  store <8 x float> %557, <8 x float>* %YMM0
  %558 = bitcast i256 %YMM1_22 to <8 x float>
  store <8 x float> %558, <8 x float>* %YMM1
  %559 = bitcast i512 %ZMM0_13 to <16 x float>
  store <16 x float> %559, <16 x float>* %ZMM0
  %560 = bitcast i512 %ZMM1_22 to <16 x float>
  store <16 x float> %560, <16 x float>* %ZMM1
  br label %bb_400666
bb_4006AB:                                        ; preds = %bb_400666
  %RIP_98 = 4196011+5
  %EIP_84 = trunc i64 %RIP_98 to i32
  %RBP_12 = load i64, i64* %RBP
  %562 = %RBP_12-64
  %563 = load double, double* %562, align 1
  %564 = bitcast double %563 to i64
  %ZMM0_14 = load <16 x float>, <16 x float>* %ZMM0
  %565 = bitcast <16 x float> %ZMM0_14 to i512
  %XMM0_14 = trunc i512 %565 to i128
  %XMM0_15 = %XMM0_14 : %564
  %568 = bitcast <16 x float> %ZMM0_14 to i512
  %YMM0_14 = trunc i512 %568 to i256
  %YMM0_15 = %YMM0_14 : %XMM0_15
  %571 = bitcast <16 x float> %ZMM0_14 to i512
  %ZMM0_15 = %571 : %XMM0_15
  %RIP_99 = %RIP_98+5
  %EIP_85 = trunc i64 %RIP_99 to i32
  %RSP_9 = load i64, i64* %RSP
  %574 = %RSP_9-8
  store i64 4196021, i64* %574
  %ESP_6 = trunc i64 %RSP_10 to i32
  store i32 %EIP_85, i32* %EIP
  store i32 %ESP_6, i32* %ESP
  store i64 %RBP_12, i64* %RBP
  store i64 %RIP_99, i64* %RIP
  store i64 %RSP_10, i64* %RSP
  %575 = bitcast i128 %XMM0_15 to <4 x float>
  store <4 x float> %575, <4 x float>* %XMM0
  %576 = bitcast i256 %YMM0_15 to <8 x float>
  store <8 x float> %576, <8 x float>* %YMM0
  %577 = bitcast i512 %ZMM0_15 to <16 x float>
  store <16 x float> %577, <16 x float>* %ZMM0
  %580 = load i64, i64* %RAX
  store i64 %580, i64* %RAX_ptr
  %581 = load i64, i64* %RBP
  store i64 %581, i64* %RBP_ptr
  %582 = load i64, i64* %RDI
  store i64 %582, i64* %RDI_ptr
  %583 = load i64, i64* %RIP
  store i64 %583, i64* %RIP_ptr
  %584 = load i64, i64* %RSP
  store i64 %584, i64* %RSP_ptr
  %585 = load <16 x float>, <16 x float>* %ZMM0
  store <16 x float> %585, <16 x float>* %ZMM0_ptr
  %586 = load <16 x float>, <16 x float>* %ZMM1
  store <16 x float> %586, <16 x float>* %ZMM1_ptr
  %587 = load <16 x float>, <16 x float>* %ZMM2
  store <16 x float> %587, <16 x float>* %ZMM2_ptr
  call void @fn_400530(%regset* %0)
  %590 = load i64, i64* %RAX_ptr
  store i64 %590, i64* %RAX
  %591 = load i64, i64* %RBP_ptr
  store i64 %591, i64* %RBP
  %592 = load i64, i64* %RDI_ptr
  store i64 %592, i64* %RDI
  %593 = load i64, i64* %RIP_ptr
  store i64 %593, i64* %RIP
  %594 = load i64, i64* %RSP_ptr
  store i64 %594, i64* %RSP
  %595 = load <16 x float>, <16 x float>* %ZMM0_ptr
  store <16 x float> %595, <16 x float>* %ZMM0
  %596 = load <16 x float>, <16 x float>* %ZMM1_ptr
  store <16 x float> %596, <16 x float>* %ZMM1
  %597 = load <16 x float>, <16 x float>* %ZMM2_ptr
  store <16 x float> %597, <16 x float>* %ZMM2
  %RIP_100 = load i64, i64* %RIP
  %RIP_101 = %RIP_100+4
  %EIP_86 = trunc i64 %RIP_101 to i32
  %ZMM0_16 = load <16 x float>, <16 x float>* %ZMM0
  %598 = bitcast <16 x float> %ZMM0_16 to i512
  %XMM0_16 = trunc i512 %598 to i128
  %599 = trunc i128 %XMM0_16 to i64
  %600 = bitcast i64 %599 to double
  %EAX_11 = fptosi double %600 to i32
  %RAX_15 = load i64, i64* %RAX
  %RAX_16 = zext i32 %EAX_11 to i64
  %601 = lshr i32 %EAX_11, 8
  %RIP_102 = %RIP_101+3
  %EIP_87 = trunc i64 %RIP_102 to i32
  %RBP_13 = load i64, i64* %RBP
  %603 = %RBP_13-4
  store i32 %EAX_11, i32* %603, align 1
  store i32 %EAX_11, i32* %EAX
  store i32 %EIP_87, i32* %EIP
  store i64 %RAX_16, i64* %RAX
  store i64 %RBP_13, i64* %RBP
  store i64 %RIP_102, i64* %RIP
  %604 = bitcast i128 %XMM0_16 to <4 x float>
  store <4 x float> %604, <4 x float>* %XMM0
  store <16 x float> %ZMM0_16, <16 x float>* %ZMM0
  br label %bb_4006BC
bb_4006BC:                                        ; preds = %bb_4006AB, %bb_400654, %bb_400623, %bb_4005EA
  %RIP_44 = 3+4196028
  %EIP_40 = trunc i64 %RIP_44 to i32
  %RBP_3 = load i64, i64* %RBP
  %606 = %RBP_3-4
  %EAX_3 = load i32, i32* %606, align 1
  %RAX_4 = load i64, i64* %RAX
  %RAX_5 = zext i32 %EAX_3 to i64
  %607 = lshr i32 %EAX_3, 8
  %RIP_45 = %RIP_44+4
  %EIP_41 = trunc i64 %RIP_45 to i32
  %RSP_3 = load i64, i64* %RSP
  %RSP_4 = %RSP_3+80
  %ESP_2 = trunc i64 %RSP_4 to i32
  %RIP_46 = %RIP_45+1
  %EIP_42 = trunc i64 %RIP_46 to i32
  %RSP_5 = %RSP_4+8
  %ESP_3 = trunc i64 %RSP_5 to i32
  %609 = %RSP_5-8
  %RBP_4 = load i64, i64* %609, align 1
  %EBP_1 = trunc i64 %RBP_4 to i32
  %RIP_47 = %RIP_46+1
  %EIP_43 = trunc i64 %RIP_47 to i32
  %610 = %RSP_5+8
  %RIP_48 = load i64, i64* %610
  %ESP_4 = trunc i64 %RSP_6 to i32
  %EIP_44 = trunc i64 %RIP_48 to i32
  %ZF_04 = icmp eq i64 %RSP_4, 0
  %SF_0 = icmp slt i64 %RSP_4, 0
  %611 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_3, i64 80)
  %OF_0 = extractvalue { i64, i1 } %611, 1
  %612 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_3, i64 80)
  %CF_05 = extractvalue { i64, i1 } %612, 1
  %613 = trunc i64 %RSP_4 to i8
  %614 = call i8 @llvm.ctpop.i8(i8 %613)
  %615 = trunc i8 %614 to i1
  %PF_06 = icmp eq i1 %615, false
  %616 = zext i1 %CF_05 to i32
  %617 = shl i32 %616, 0
  %619 = zext i1 %PF_06 to i32
  %620 = shl i32 %619, 2
  %622 = zext i1 false to i32
  %623 = shl i32 %622, 4
  %625 = zext i1 %ZF_04 to i32
  %626 = shl i32 %625, 6
  %628 = zext i1 %SF_0 to i32
  %629 = shl i32 %628, 7
  %631 = zext i1 %OF_0 to i32
  %632 = shl i32 %631, 11
  store i32 %EAX_3, i32* %EAX
  store i32 %EBP_1, i32* %EBP
  store i32 %EIP_44, i32* %EIP
  store i32 %ESP_4, i32* %ESP
  store i64 %RAX_5, i64* %RAX
  store i64 %RBP_4, i64* %RBP
  store i64 %RIP_48, i64* %RIP
  store i64 %RSP_6, i64* %RSP
  br label %exit_fn_400550
}
