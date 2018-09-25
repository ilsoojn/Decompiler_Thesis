define void @fn_400550(%regset* noalias nocapture) {
entry_fn_400550:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %regset*
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %regset*
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
%RBP_init = %regset*
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
%RSP_init = %regset*
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = %regset*
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = %regset*
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = %regset*
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%1 = %regset*
%2 = trunc i512 %1 to i128
%XMM0_init = %regset*
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%3 = %regset*
%4 = trunc i512 %3 to i256
%YMM0_init = %regset*
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %regset*
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %regset*
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%5 = lshr i64 %regset*, 8
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %regset*
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%6 = %regset*
%7 = trunc i512 %6 to i128
%XMM1_init = %regset*
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %regset*
%9 = trunc i512 %8 to i256
%YMM1_init = %regset*
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%ZMM2_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 87
%ZMM2_init = %regset*
%ZMM2 = alloca <16 x float>
store <16 x float> %ZMM2_init, <16 x float>* %ZMM2
%10 = %regset*
%11 = trunc i512 %10 to i128
%XMM2_init = %regset*
%XMM2 = alloca <4 x float>
store <4 x float> %XMM2_init, <4 x float>* %XMM2
%12 = %regset*
%13 = trunc i512 %12 to i256
%YMM2_init = %regset*
%YMM2 = alloca <8 x float>
store <8 x float> %YMM2_init, <8 x float>* %YMM2
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = %regset*
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = %regset*
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
%RIP_1 = 4195665
%EIP_0 = 4195665
%RBP_0 = %RBP
%RSP_0 = %RSP
%25 = %RSP-8
store i64 %RBP_0, i64* %25, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195668
%EIP_1 = 4195668
%EBP_0 = %RSP-8
%RIP_3 = 4195672
%EIP_2 = 4195672
%RSP_2 = %RSP-88
%ESP_1 = %RSP-88
%RIP_4 = 4195675
%EIP_3 = 4195675
%ZMM0_0 = %ZMM0
%26 = %ZMM0
%XMM0_0 = %ZMM0
%27 = %ZMM0
%28 = bitcast <4 x float> %27 to <2 x i64>
%29 = %ZMM0
%30 = bitcast <4 x float> %29 to <2 x i64>
%31 = xor <4 x float> %27, %29
%XMM0_1 = %31
%32 = %ZMM0
%YMM0_0 = %ZMM0
%33 = %31
%34 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_1 = or i256 %33, %34
%35 = %ZMM0
%36 = %31
%37 = and i512 %35, -340282366920938463463374607431768211456
%ZMM0_1 = or i512 %36, %37
%RIP_5 = 4195685
%EIP_4 = 4195685
%RIP_6 = 4195690
%EIP_5 = 4195690
%38 = sitofp i64 4003244811 to double
%39 = bitcast i64 4003244811 to i64
%ZMM1_0 = %ZMM1
%40 = %ZMM1
%XMM1_0 = %ZMM1
%41 = zext i64 4003244811 to i128
%42 = and i128 %ZMM1, -18446744073709551616
%XMM1_1 = or i128 %41, %42
%43 = %ZMM1
%YMM1_0 = %ZMM1
%44 = zext i128 %XMM1_1 to i256
%45 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_1 = or i256 %44, %45
%46 = %ZMM1
%47 = zext i128 %XMM1_1 to i512
%48 = and i512 %46, -340282366920938463463374607431768211456
%ZMM1_1 = or i512 %47, %48
%RIP_7 = 4195700
%EIP_6 = 4195700
%RIP_8 = 4195705
%EIP_7 = 4195705
%49 = sitofp i64 1336630430 to double
%50 = bitcast i64 1336630430 to i64
%ZMM2_0 = %ZMM2
%51 = %ZMM2
%XMM2_0 = %ZMM2
%52 = zext i64 1336630430 to i128
%53 = and i128 %ZMM2, -18446744073709551616
%XMM2_1 = or i128 %52, %53
%54 = %ZMM2
%YMM2_0 = %ZMM2
%55 = zext i128 %XMM2_1 to i256
%56 = and i256 %ZMM2, -340282366920938463463374607431768211456
%YMM2_1 = or i256 %55, %56
%57 = %ZMM2
%58 = zext i128 %XMM2_1 to i512
%59 = and i512 %57, -340282366920938463463374607431768211456
%ZMM2_1 = or i512 %58, %59
%RIP_9 = 4195712
%EIP_8 = 4195712
%61 = %RSP-8-4
store i32 0, i32* %61, align 1
%RIP_10 = 4195717
%EIP_9 = 4195717
%62 = trunc i128 %XMM2_1 to i64
%63 = bitcast i128 %XMM2_1 to double
%65 = %RSP-8-24
store i128 %XMM2_1, double* %65, align 1
%RIP_11 = 4195722
%EIP_10 = 4195722
%66 = trunc i128 %XMM1_1 to i64
%67 = bitcast i128 %XMM1_1 to double
%69 = %RSP-8-16
store i128 %XMM1_1, double* %69, align 1
%RIP_12 = 4195727
%EIP_11 = 4195727
%71 = %RSP-8-16
%72 = load double, double* %71, align 1
%73 = bitcast double %72 to i64
%74 = zext double %72 to i128
%75 = and i128 %XMM1_1, -18446744073709551616
%XMM1_2 = or i128 %74, %75
%76 = zext i128 %XMM1_2 to i256
%77 = and i256 %YMM1_1, -340282366920938463463374607431768211456
%YMM1_2 = or i256 %76, %77
%78 = zext i128 %XMM1_2 to i512
%79 = and i512 %ZMM1_1, -340282366920938463463374607431768211456
%ZMM1_2 = or i512 %78, %79
%RIP_13 = 4195732
%EIP_12 = 4195732
%80 = trunc i128 %XMM1_2 to i64
%81 = bitcast i128 %XMM1_2 to double
%83 = %RSP-8-24
%84 = load double, double* %83, align 1
%85 = fadd i128 %XMM1_2, %84
%86 = bitcast double %85 to i64
%87 = zext double %85 to i128
%88 = and i128 %XMM1_2, -18446744073709551616
%XMM1_3 = or i128 %87, %88
%89 = zext i128 %XMM1_3 to i256
%90 = and i256 %YMM1_2, -340282366920938463463374607431768211456
%YMM1_3 = or i256 %89, %90
%91 = zext i128 %XMM1_3 to i512
%92 = and i512 %ZMM1_2, -340282366920938463463374607431768211456
%ZMM1_3 = or i512 %91, %92
%RIP_14 = 4195737
%EIP_13 = 4195737
%93 = trunc i128 %XMM1_3 to i64
%94 = bitcast i128 %XMM1_3 to double
%96 = %RSP-8-32
store i128 %XMM1_3, double* %96, align 1
%RIP_15 = 4195742
%EIP_14 = 4195742
%98 = %RSP-8-16
%99 = load double, double* %98, align 1
%100 = bitcast double %99 to i64
%101 = zext double %99 to i128
%102 = and i128 %XMM1_3, -18446744073709551616
%XMM1_4 = or i128 %101, %102
%103 = zext i128 %XMM1_4 to i256
%104 = and i256 %YMM1_3, -340282366920938463463374607431768211456
%YMM1_4 = or i256 %103, %104
%105 = zext i128 %XMM1_4 to i512
%106 = and i512 %ZMM1_3, -340282366920938463463374607431768211456
%ZMM1_4 = or i512 %105, %106
%RIP_16 = 4195747
%EIP_15 = 4195747
%107 = trunc i128 %XMM1_4 to i64
%108 = bitcast i128 %XMM1_4 to double
%110 = %RSP-8-24
%111 = load double, double* %110, align 1
%112 = fsub i128 %XMM1_4, %111
%113 = bitcast double %112 to i64
%114 = zext double %112 to i128
%115 = and i128 %XMM1_4, -18446744073709551616
%XMM1_5 = or i128 %114, %115
%116 = zext i128 %XMM1_5 to i256
%117 = and i256 %YMM1_4, -340282366920938463463374607431768211456
%YMM1_5 = or i256 %116, %117
%118 = zext i128 %XMM1_5 to i512
%119 = and i512 %ZMM1_4, -340282366920938463463374607431768211456
%ZMM1_5 = or i512 %118, %119
%RIP_17 = 4195752
%EIP_16 = 4195752
%120 = trunc i128 %XMM1_5 to i64
%121 = bitcast i128 %XMM1_5 to double
%123 = %RSP-8-40
store i128 %XMM1_5, double* %123, align 1
%RIP_18 = 4195757
%EIP_17 = 4195757
%125 = %RSP-8-16
%126 = load double, double* %125, align 1
%127 = bitcast double %126 to i64
%128 = zext double %126 to i128
%129 = and i128 %XMM1_5, -18446744073709551616
%XMM1_6 = or i128 %128, %129
%130 = zext i128 %XMM1_6 to i256
%131 = and i256 %YMM1_5, -340282366920938463463374607431768211456
%YMM1_6 = or i256 %130, %131
%132 = zext i128 %XMM1_6 to i512
%133 = and i512 %ZMM1_5, -340282366920938463463374607431768211456
%ZMM1_6 = or i512 %132, %133
%RIP_19 = 4195762
%EIP_18 = 4195762
%134 = trunc i128 %XMM1_6 to i64
%135 = bitcast i128 %XMM1_6 to double
%137 = %RSP-8-24
%138 = load double, double* %137, align 1
%139 = fmul i128 %XMM1_6, %138
%140 = bitcast double %139 to i64
%141 = zext double %139 to i128
%142 = and i128 %XMM1_6, -18446744073709551616
%XMM1_7 = or i128 %141, %142
%143 = zext i128 %XMM1_7 to i256
%144 = and i256 %YMM1_6, -340282366920938463463374607431768211456
%YMM1_7 = or i256 %143, %144
%145 = zext i128 %XMM1_7 to i512
%146 = and i512 %ZMM1_6, -340282366920938463463374607431768211456
%ZMM1_7 = or i512 %145, %146
%RIP_20 = 4195767
%EIP_19 = 4195767
%147 = trunc i128 %XMM1_7 to i64
%148 = bitcast i128 %XMM1_7 to double
%150 = %RSP-8-48
store i128 %XMM1_7, double* %150, align 1
%RIP_21 = 4195772
%EIP_20 = 4195772
%152 = %RSP-8-16
%153 = load double, double* %152, align 1
%154 = bitcast double %153 to i64
%155 = zext double %153 to i128
%156 = and i128 %XMM1_7, -18446744073709551616
%XMM1_8 = or i128 %155, %156
%157 = zext i128 %XMM1_8 to i256
%158 = and i256 %YMM1_7, -340282366920938463463374607431768211456
%YMM1_8 = or i256 %157, %158
%159 = zext i128 %XMM1_8 to i512
%160 = and i512 %ZMM1_7, -340282366920938463463374607431768211456
%ZMM1_8 = or i512 %159, %160
%RIP_22 = 4195777
%EIP_21 = 4195777
%161 = trunc i128 %XMM1_8 to i64
%162 = bitcast i128 %XMM1_8 to double
%164 = %RSP-8-24
%165 = load double, double* %164, align 1
%166 = fdiv i128 %XMM1_8, %165
%167 = bitcast double %166 to i64
%168 = zext double %166 to i128
%169 = and i128 %XMM1_8, -18446744073709551616
%XMM1_9 = or i128 %168, %169
%170 = zext i128 %XMM1_9 to i256
%171 = and i256 %YMM1_8, -340282366920938463463374607431768211456
%YMM1_9 = or i256 %170, %171
%172 = zext i128 %XMM1_9 to i512
%173 = and i512 %ZMM1_8, -340282366920938463463374607431768211456
%ZMM1_9 = or i512 %172, %173
%RIP_23 = 4195782
%EIP_22 = 4195782
%174 = trunc i128 %XMM1_9 to i64
%175 = bitcast i128 %XMM1_9 to double
%177 = %RSP-8-56
store i128 %XMM1_9, double* %177, align 1
%RIP_24 = 4195787
%EIP_23 = 4195787
%179 = %RSP-8-32
%180 = load double, double* %179, align 1
%181 = bitcast double %180 to i64
%182 = zext double %180 to i128
%183 = and i128 %XMM1_9, -18446744073709551616
%XMM1_10 = or i128 %182, %183
%184 = zext i128 %XMM1_10 to i256
%185 = and i256 %YMM1_9, -340282366920938463463374607431768211456
%YMM1_10 = or i256 %184, %185
%186 = zext i128 %XMM1_10 to i512
%187 = and i512 %ZMM1_9, -340282366920938463463374607431768211456
%ZMM1_10 = or i512 %186, %187
%RIP_25 = 4195792
%EIP_24 = 4195792
%188 = trunc i128 %XMM1_10 to i64
%189 = bitcast i128 %XMM1_10 to double
%191 = %RSP-8-40
%192 = load double, double* %191, align 1
%193 = fmul i128 %XMM1_10, %192
%194 = bitcast double %193 to i64
%195 = zext double %193 to i128
%196 = and i128 %XMM1_10, -18446744073709551616
%XMM1_11 = or i128 %195, %196
%197 = zext i128 %XMM1_11 to i256
%198 = and i256 %YMM1_10, -340282366920938463463374607431768211456
%YMM1_11 = or i256 %197, %198
%199 = zext i128 %XMM1_11 to i512
%200 = and i512 %ZMM1_10, -340282366920938463463374607431768211456
%ZMM1_11 = or i512 %199, %200
%RIP_26 = 4195797
%EIP_25 = 4195797
%201 = trunc i128 %XMM1_11 to i64
%202 = bitcast i128 %XMM1_11 to double
%204 = %RSP-8-48
%205 = load double, double* %204, align 1
%206 = fdiv i128 %XMM1_11, %205
%207 = bitcast double %206 to i64
%208 = zext double %206 to i128
%209 = and i128 %XMM1_11, -18446744073709551616
%XMM1_12 = or i128 %208, %209
%210 = zext i128 %XMM1_12 to i256
%211 = and i256 %YMM1_11, -340282366920938463463374607431768211456
%YMM1_12 = or i256 %210, %211
%212 = zext i128 %XMM1_12 to i512
%213 = and i512 %ZMM1_11, -340282366920938463463374607431768211456
%ZMM1_12 = or i512 %212, %213
%RIP_27 = 4195802
%EIP_26 = 4195802
%214 = trunc i128 %XMM1_12 to i64
%215 = bitcast i128 %XMM1_12 to double
%217 = %RSP-8-56
%218 = load double, double* %217, align 1
%219 = fadd i128 %XMM1_12, %218
%220 = bitcast double %219 to i64
%221 = zext double %219 to i128
%222 = and i128 %XMM1_12, -18446744073709551616
%XMM1_13 = or i128 %221, %222
%223 = zext i128 %XMM1_13 to i256
%224 = and i256 %YMM1_12, -340282366920938463463374607431768211456
%YMM1_13 = or i256 %223, %224
%225 = zext i128 %XMM1_13 to i512
%226 = and i512 %ZMM1_12, -340282366920938463463374607431768211456
%ZMM1_13 = or i512 %225, %226
%RIP_28 = 4195807
%EIP_27 = 4195807
%227 = trunc i128 %XMM1_13 to i64
%228 = bitcast i128 %XMM1_13 to double
%230 = %RSP-8-64
store i128 %XMM1_13, double* %230, align 1
%RIP_29 = 4195812
%EIP_28 = 4195812
%231 = %31
%232 = bitcast i64 %231 to double
%234 = %RSP-8-64
%235 = load double, double* %234, align 1
%ZF_0 = fcmp ueq double %232, %235
%PF_0 = fcmp uno i64 %231, %235
%CF_0 = fcmp ult i64 %231, %235
%236 = zext i1 %CF_0 to i32
%237 = shl i1 %CF_0, 0
%239 = zext i1 %PF_0 to i32
%240 = shl i1 %PF_0, 2
%242 = zext i1 false to i32
%243 = shl i1 false, 4
%245 = zext i1 %ZF_0 to i32
%246 = shl i1 %ZF_0, 6
%248 = zext i1 false to i32
%249 = shl i1 false, 7
%251 = zext i1 false to i32
%252 = shl i1 false, 11
%RIP_30 = 4195818
%EIP_29 = 4195818
%CC_BE_0 = or i1 %CF_0, %ZF_0
store i32 1336630430, i32* %EAX
store i32 %EBP_0, i32* %EBP
store i32 4195831, i32* %EIP
store i32 %ESP_1, i32* %ESP
store i64 1336630430, i64* %RAX
store i64 %RSP_1, i64* %RBP
store i64 4195831, i64* %RIP
store i64 %RSP_2, i64* %RSP
%253 = %31
store <4 x float> %253, <4 x float>* %XMM0
%254 = bitcast i128 %XMM1_13 to <4 x float>
store i128 %XMM1_13, <4 x float>* %XMM1
%255 = bitcast i128 %XMM2_1 to <4 x float>
store i128 %XMM2_1, <4 x float>* %XMM2
%256 = bitcast i256 %YMM0_1 to <8 x float>
store i256 %YMM0_1, <8 x float>* %YMM0
%257 = bitcast i256 %YMM1_13 to <8 x float>
store i256 %YMM1_13, <8 x float>* %YMM1
%258 = bitcast i256 %YMM2_1 to <8 x float>
store i256 %YMM2_1, <8 x float>* %YMM2
%259 = bitcast i512 %ZMM0_1 to <16 x float>
store i512 %ZMM0_1, <16 x float>* %ZMM0
%260 = bitcast i512 %ZMM1_13 to <16 x float>
store i512 %ZMM1_13, <16 x float>* %ZMM1
%261 = bitcast i512 %ZMM2_1 to <16 x float>
store i512 %ZMM2_1, <16 x float>* %ZMM2
br i1 %CC_BE_0, label %bb_4005F7, label %bb_4005EA
bb_4005EA:                                        ; preds = %bb_400550
%RIP_33 = 4195823
%EIP_31 = 4195823
%RBP_1 = %RBP
%263 = %RBP-32
%264 = load double, double* %263, align 1
%EAX_2 = %RBP-32
%RAX_2 = %RAX
%RAX_3 = %RBP-32
%265 = lshr i32 %RBP-32, 8
%RIP_34 = 4195826
%EIP_32 = 4195826
%267 = %RBP-4
store i32 %EAX_2, i32* %267, align 1
%RIP_35 = 4195831
%EIP_33 = 4195831
store i32 %EAX_2, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_3, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_4005F7:                                        ; preds = %bb_400550
%RIP_38 = 4195834
%EIP_35 = 4195834
%ZMM0_2 = %ZMM0
%268 = %ZMM0
%XMM0_2 = %ZMM0
%269 = %ZMM0
%270 = bitcast <4 x float> %269 to <2 x i64>
%271 = %ZMM0
%272 = bitcast <4 x float> %271 to <2 x i64>
%273 = xor <4 x float> %269, %271
%XMM0_3 = %273
%274 = %ZMM0
%YMM0_2 = %ZMM0
%275 = %273
%276 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_3 = or i256 %275, %276
%277 = %ZMM0
%278 = %273
%279 = and i512 %277, -340282366920938463463374607431768211456
%ZMM0_3 = or i512 %278, %279
%RIP_39 = 4195839
%EIP_36 = 4195839
%RBP_2 = %RBP
%281 = %RBP-64
%282 = load double, double* %281, align 1
%283 = bitcast double %282 to i64
%ZMM1_14 = %ZMM1
%284 = %ZMM1
%XMM1_14 = %ZMM1
%285 = zext double %282 to i128
%286 = and i128 %ZMM1, -18446744073709551616
%XMM1_15 = or i128 %285, %286
%287 = %ZMM1
%YMM1_14 = %ZMM1
%288 = zext i128 %XMM1_15 to i256
%289 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_15 = or i256 %288, %289
%290 = %ZMM1
%291 = zext i128 %XMM1_15 to i512
%292 = and i512 %290, -340282366920938463463374607431768211456
%ZMM1_15 = or i512 %291, %292
%RIP_40 = 4195843
%EIP_37 = 4195843
%293 = trunc i128 %XMM1_15 to i64
%294 = bitcast i128 %XMM1_15 to double
%295 = %273
%296 = bitcast i64 %295 to double
%ZF_01 = fcmp ueq double %294, %296
%PF_02 = fcmp uno i128 %XMM1_15, %295
%CF_03 = fcmp ult i128 %XMM1_15, %295
%297 = zext i1 %CF_03 to i32
%298 = shl i1 %CF_03, 0
%300 = zext i1 %PF_02 to i32
%301 = shl i1 %PF_02, 2
%303 = zext i1 false to i32
%304 = shl i1 false, 4
%306 = zext i1 %ZF_01 to i32
%307 = shl i1 %ZF_01, 6
%309 = zext i1 false to i32
%310 = shl i1 false, 7
%312 = zext i1 false to i32
%313 = shl i1 false, 11
%RIP_41 = 4195849
%EIP_38 = 4195849
store i32 4195888, i32* %EIP
store i64 %RBP_2, i64* %RBP
store i64 4195888, i64* %RIP
%314 = %273
store <4 x float> %314, <4 x float>* %XMM0
%315 = bitcast i128 %XMM1_15 to <4 x float>
store i128 %XMM1_15, <4 x float>* %XMM1
%316 = bitcast i256 %YMM0_3 to <8 x float>
store i256 %YMM0_3, <8 x float>* %YMM0
%317 = bitcast i256 %YMM1_15 to <8 x float>
store i256 %YMM1_15, <8 x float>* %YMM1
%318 = bitcast i512 %ZMM0_3 to <16 x float>
store i512 %ZMM0_3, <16 x float>* %ZMM0
%319 = bitcast i512 %ZMM1_15 to <16 x float>
store i512 %ZMM1_15, <16 x float>* %ZMM1
br i1 %CF_03, label %bb_400630, label %bb_400609
bb_400609:                                        ; preds = %bb_4005F7
%RIP_50 = 4195859
%EIP_45 = 4195859
%RIP_51 = 4195864
%EIP_46 = 4195864
%320 = sitofp i64 5 to double
%321 = bitcast i64 5 to i64
%ZMM0_4 = %ZMM0
%322 = %ZMM0
%XMM0_4 = %ZMM0
%323 = zext i64 5 to i128
%324 = and i128 %ZMM0, -18446744073709551616
%XMM0_5 = or i128 %323, %324
%325 = %ZMM0
%YMM0_4 = %ZMM0
%326 = zext i128 %XMM0_5 to i256
%327 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_5 = or i256 %326, %327
%328 = %ZMM0
%329 = zext i128 %XMM0_5 to i512
%330 = and i512 %328, -340282366920938463463374607431768211456
%ZMM0_5 = or i512 %329, %330
%RIP_52 = 4195869
%EIP_47 = 4195869
%331 = trunc i128 %XMM0_5 to i64
%332 = bitcast i128 %XMM0_5 to double
%RBP_5 = %RBP
%334 = %RBP-64
%335 = load double, double* %334, align 1
%ZF_07 = fcmp ueq double %332, %335
%PF_08 = fcmp uno i128 %XMM0_5, %335
%CF_09 = fcmp ult i128 %XMM0_5, %335
%336 = zext i1 %CF_09 to i32
%337 = shl i1 %CF_09, 0
%339 = zext i1 %PF_08 to i32
%340 = shl i1 %PF_08, 2
%342 = zext i1 false to i32
%343 = shl i1 false, 4
%345 = zext i1 %ZF_07 to i32
%346 = shl i1 %ZF_07, 6
%348 = zext i1 false to i32
%349 = shl i1 false, 7
%351 = zext i1 false to i32
%352 = shl i1 false, 11
%RIP_53 = 4195875
%EIP_48 = 4195875
%CC_BE_010 = or i1 %CF_09, %ZF_07
store i32 5, i32* %EAX
store i32 4195888, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 4195888, i64* %RIP
%353 = bitcast i128 %XMM0_5 to <4 x float>
store i128 %XMM0_5, <4 x float>* %XMM0
%354 = bitcast i256 %YMM0_5 to <8 x float>
store i256 %YMM0_5, <8 x float>* %YMM0
%355 = bitcast i512 %ZMM0_5 to <16 x float>
store i512 %ZMM0_5, <16 x float>* %ZMM0
br i1 %CC_BE_010, label %bb_400630, label %bb_400623
bb_400623:                                        ; preds = %bb_400609
%RIP_63 = 4195880
%EIP_56 = 4195880
%RBP_7 = %RBP
%357 = %RBP-40
%358 = load double, double* %357, align 1
%EAX_6 = %RBP-40
%RAX_8 = %RAX
%RAX_9 = %RBP-40
%359 = lshr i32 %RBP-40, 8
%RIP_64 = 4195883
%EIP_57 = 4195883
%361 = %RBP-4
store i32 %EAX_6, i32* %361, align 1
%RIP_65 = 4195888
%EIP_58 = 4195888
store i32 %EAX_6, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_9, i64* %RAX
store i64 %RBP_7, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_400630:                                        ; preds = %bb_400609, %bb_4005F7
%RIP_56 = 4195898
%EIP_50 = 4195898
%RIP_57 = 4195903
%EIP_51 = 4195903
%362 = sitofp i64 5 to double
%363 = bitcast i64 5 to i64
%ZMM0_6 = %ZMM0
%364 = %ZMM0
%XMM0_6 = %ZMM0
%365 = zext i64 5 to i128
%366 = and i128 %ZMM0, -18446744073709551616
%XMM0_7 = or i128 %365, %366
%367 = %ZMM0
%YMM0_6 = %ZMM0
%368 = zext i128 %XMM0_7 to i256
%369 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_7 = or i256 %368, %369
%370 = %ZMM0
%371 = zext i128 %XMM0_7 to i512
%372 = and i512 %370, -340282366920938463463374607431768211456
%ZMM0_7 = or i512 %371, %372
%RIP_58 = 4195908
%EIP_52 = 4195908
%RBP_6 = %RBP
%374 = %RBP-64
%375 = load double, double* %374, align 1
%376 = bitcast double %375 to i64
%ZMM1_16 = %ZMM1
%377 = %ZMM1
%XMM1_16 = %ZMM1
%378 = zext double %375 to i128
%379 = and i128 %ZMM1, -18446744073709551616
%XMM1_17 = or i128 %378, %379
%380 = %ZMM1
%YMM1_16 = %ZMM1
%381 = zext i128 %XMM1_17 to i256
%382 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_17 = or i256 %381, %382
%383 = %ZMM1
%384 = zext i128 %XMM1_17 to i512
%385 = and i512 %383, -340282366920938463463374607431768211456
%ZMM1_17 = or i512 %384, %385
%RIP_59 = 4195912
%EIP_53 = 4195912
%386 = trunc i128 %XMM1_17 to i64
%387 = bitcast i128 %XMM1_17 to double
%388 = trunc i128 %XMM0_7 to i64
%389 = bitcast i128 %XMM0_7 to double
%ZF_011 = fcmp ueq double %387, %389
%PF_012 = fcmp uno i128 %XMM1_17, %XMM0_7
%CF_013 = fcmp ult i128 %XMM1_17, %XMM0_7
%390 = zext i1 %CF_013 to i32
%391 = shl i1 %CF_013, 0
%393 = zext i1 %PF_012 to i32
%394 = shl i1 %PF_012, 2
%396 = zext i1 false to i32
%397 = shl i1 false, 4
%399 = zext i1 %ZF_011 to i32
%400 = shl i1 %ZF_011, 6
%402 = zext i1 false to i32
%403 = shl i1 false, 7
%405 = zext i1 false to i32
%406 = shl i1 false, 11
%RIP_60 = 4195918
%EIP_54 = 4195918
%CC_NE_0 = xor i1 %ZF_011, true
store i32 5, i32* %EAX
store i32 4195937, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195937, i64* %RIP
%407 = bitcast i128 %XMM0_7 to <4 x float>
store i128 %XMM0_7, <4 x float>* %XMM0
%408 = bitcast i128 %XMM1_17 to <4 x float>
store i128 %XMM1_17, <4 x float>* %XMM1
%409 = bitcast i256 %YMM0_7 to <8 x float>
store i256 %YMM0_7, <8 x float>* %YMM0
%410 = bitcast i256 %YMM1_17 to <8 x float>
store i256 %YMM1_17, <8 x float>* %YMM1
%411 = bitcast i512 %ZMM0_7 to <16 x float>
store i512 %ZMM0_7, <16 x float>* %ZMM0
%412 = bitcast i512 %ZMM1_17 to <16 x float>
store i512 %ZMM1_17, <16 x float>* %ZMM1
br i1 %CC_NE_0, label %bb_400661, label %bb_40064E
bb_40064E:                                        ; preds = %bb_400630
%RIP_68 = 4195924
%EIP_60 = 4195924
store i32 4195937, i32* %EIP
store i64 4195937, i64* %RIP
bb_400654:                                        ; preds = %bb_40064E
%RIP_74 = 4195929
%EIP_64 = 4195929
%RBP_8 = %RBP
%415 = %RBP-48
%416 = load double, double* %415, align 1
%EAX_7 = %RBP-48
%RAX_10 = %RAX
%RAX_11 = %RBP-48
%417 = lshr i32 %RBP-48, 8
%RIP_75 = 4195932
%EIP_65 = 4195932
%419 = %RBP-4
store i32 %EAX_7, i32* %419, align 1
%RIP_76 = 4195937
%EIP_66 = 4195937
store i32 %EAX_7, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_11, i64* %RAX
store i64 %RBP_8, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_400661:                                        ; preds = %bb_40064E, %bb_400630
%RIP_71 = 4195942
%EIP_62 = 4195942
store i32 4195942, i32* %EIP
store i64 4195942, i64* %RIP
br label %bb_400666
bb_400666:                                        ; preds = %bb_400678, %bb_400661
%RIP_79 = 4195945
%EIP_68 = 4195945
%ZMM0_8 = %ZMM0
%420 = %ZMM0
%XMM0_8 = %ZMM0
%421 = %ZMM0
%422 = bitcast <4 x float> %421 to <2 x i64>
%423 = %ZMM0
%424 = bitcast <4 x float> %423 to <2 x i64>
%425 = xor <4 x float> %421, %423
%XMM0_9 = %425
%426 = %ZMM0
%YMM0_8 = %ZMM0
%427 = %425
%428 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_9 = or i256 %427, %428
%429 = %ZMM0
%430 = %425
%431 = and i512 %429, -340282366920938463463374607431768211456
%ZMM0_9 = or i512 %430, %431
%RIP_80 = 4195950
%EIP_69 = 4195950
%RBP_9 = %RBP
%433 = %RBP-64
%434 = load double, double* %433, align 1
%435 = bitcast double %434 to i64
%ZMM1_18 = %ZMM1
%436 = %ZMM1
%XMM1_18 = %ZMM1
%437 = zext double %434 to i128
%438 = and i128 %ZMM1, -18446744073709551616
%XMM1_19 = or i128 %437, %438
%439 = %ZMM1
%YMM1_18 = %ZMM1
%440 = zext i128 %XMM1_19 to i256
%441 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_19 = or i256 %440, %441
%442 = %ZMM1
%443 = zext i128 %XMM1_19 to i512
%444 = and i512 %442, -340282366920938463463374607431768211456
%ZMM1_19 = or i512 %443, %444
%RIP_81 = 4195954
%EIP_70 = 4195954
%445 = trunc i128 %XMM1_19 to i64
%446 = bitcast i128 %XMM1_19 to double
%447 = %425
%448 = bitcast i64 %447 to double
%ZF_015 = fcmp ueq double %446, %448
%PF_016 = fcmp uno i128 %XMM1_19, %447
%CF_017 = fcmp ult i128 %XMM1_19, %447
%449 = zext i1 %CF_017 to i32
%450 = shl i1 %CF_017, 0
%452 = zext i1 %PF_016 to i32
%453 = shl i1 %PF_016, 2
%455 = zext i1 false to i32
%456 = shl i1 false, 4
%458 = zext i1 %ZF_015 to i32
%459 = shl i1 %ZF_015, 6
%461 = zext i1 false to i32
%462 = shl i1 false, 7
%464 = zext i1 false to i32
%465 = shl i1 false, 11
%RIP_82 = 4195960
%EIP_71 = 4195960
%CC_BE_018 = or i1 %CF_017, %ZF_015
store i32 4196011, i32* %EIP
store i64 %RBP_9, i64* %RBP
store i64 4196011, i64* %RIP
%466 = %425
store <4 x float> %466, <4 x float>* %XMM0
%467 = bitcast i128 %XMM1_19 to <4 x float>
store i128 %XMM1_19, <4 x float>* %XMM1
%468 = bitcast i256 %YMM0_9 to <8 x float>
store i256 %YMM0_9, <8 x float>* %YMM0
%469 = bitcast i256 %YMM1_19 to <8 x float>
store i256 %YMM1_19, <8 x float>* %YMM1
%470 = bitcast i512 %ZMM0_9 to <16 x float>
store i512 %ZMM0_9, <16 x float>* %ZMM0
%471 = bitcast i512 %ZMM1_19 to <16 x float>
store i512 %ZMM1_19, <16 x float>* %ZMM1
br i1 %CC_BE_018, label %bb_4006AB, label %bb_400678
bb_400678:                                        ; preds = %bb_400666
%RIP_85 = 4195968
%EIP_73 = 4195968
%RIP_86 = 4195973
%EIP_74 = 4195973
%RBP_10 = %RBP
%473 = %RBP-64
%474 = load double, double* %473, align 1
%475 = bitcast double %474 to i64
%ZMM0_10 = %ZMM0
%476 = %ZMM0
%XMM0_10 = %ZMM0
%477 = zext double %474 to i128
%478 = and i128 %ZMM0, -18446744073709551616
%XMM0_11 = or i128 %477, %478
%479 = %ZMM0
%YMM0_10 = %ZMM0
%480 = zext i128 %XMM0_11 to i256
%481 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_11 = or i256 %480, %481
%482 = %ZMM0
%483 = zext i128 %XMM0_11 to i512
%484 = and i512 %482, -340282366920938463463374607431768211456
%ZMM0_11 = or i512 %483, %484
%RIP_87 = 4195975
%EIP_75 = 4195975
%RAX_12 = %RAX
%EAX_8 = %RAX
%486 = and i32 %RAX, -256
%EAX_9 = or i32 %486, 1
%487 = and i64 %RAX, -256
%RAX_13 = or i64 %487, 1
%RIP_88 = 4195980
%EIP_76 = 4195980
%RSP_7 = %RSP
%488 = %RSP-8
store i64 4195980, i64* %488
%ESP_5 = %RSP-8
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
store i128 %XMM0_11, <4 x float>* %XMM0
%490 = bitcast i256 %YMM0_11 to <8 x float>
store i256 %YMM0_11, <8 x float>* %YMM0
%491 = bitcast i512 %ZMM0_11 to <16 x float>
store i512 %ZMM0_11, <16 x float>* %ZMM0
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
%RIP_89 = %RIP
%RIP_90 = %RIP+9
%EIP_77 = %RIP+9
%512 = inttoptr i64 4196184 to double*
%513 = load double, i64 4196184, align 1
%514 = bitcast double %513 to i64
%ZMM0_12 = %ZMM0
%515 = %ZMM0
%XMM0_12 = %ZMM0
%516 = zext double %513 to i128
%517 = and i128 %ZMM0, -18446744073709551616
%XMM0_13 = or i128 %516, %517
%518 = %ZMM0
%YMM0_12 = %ZMM0
%519 = zext i128 %XMM0_13 to i256
%520 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_13 = or i256 %519, %520
%521 = %ZMM0
%522 = zext i128 %XMM0_13 to i512
%523 = and i512 %521, -340282366920938463463374607431768211456
%ZMM0_13 = or i512 %522, %523
%RIP_91 = %RIP+14
%EIP_78 = %RIP+14
%RBP_11 = %RBP
%525 = %RBP-64
%526 = load double, double* %525, align 1
%527 = bitcast double %526 to i64
%ZMM1_20 = %ZMM1
%528 = %ZMM1
%XMM1_20 = %ZMM1
%529 = zext double %526 to i128
%530 = and i128 %ZMM1, -18446744073709551616
%XMM1_21 = or i128 %529, %530
%531 = %ZMM1
%YMM1_20 = %ZMM1
%532 = zext i128 %XMM1_21 to i256
%533 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_21 = or i256 %532, %533
%534 = %ZMM1
%535 = zext i128 %XMM1_21 to i512
%536 = and i512 %534, -340282366920938463463374607431768211456
%ZMM1_21 = or i512 %535, %536
%RIP_92 = %RIP+18
%EIP_79 = %RIP+18
%537 = trunc i128 %XMM1_21 to i64
%538 = bitcast i128 %XMM1_21 to double
%539 = trunc i128 %XMM0_13 to i64
%540 = bitcast i128 %XMM0_13 to double
%541 = fsub i128 %XMM1_21, %XMM0_13
%542 = bitcast double %541 to i64
%543 = zext double %541 to i128
%544 = and i128 %XMM1_21, -18446744073709551616
%XMM1_22 = or i128 %543, %544
%545 = zext i128 %XMM1_22 to i256
%546 = and i256 %YMM1_21, -340282366920938463463374607431768211456
%YMM1_22 = or i256 %545, %546
%547 = zext i128 %XMM1_22 to i512
%548 = and i512 %ZMM1_21, -340282366920938463463374607431768211456
%ZMM1_22 = or i512 %547, %548
%RIP_93 = %RIP+23
%EIP_80 = %RIP+23
%549 = trunc i128 %XMM1_22 to i64
%550 = bitcast i128 %XMM1_22 to double
%552 = %RBP-64
store i128 %XMM1_22, double* %552, align 1
%RIP_94 = %RIP+26
%EIP_81 = %RIP+26
%RAX_14 = %RAX
%EAX_10 = %RAX
%554 = %RBP-68
store i32 %EAX_10, i32* %554, align 1
%RIP_95 = %RIP+31
%EIP_82 = %RIP+31
store i32 %EAX_10, i32* %EAX
store i32 4195942, i32* %EIP
store i64 %RAX_14, i64* %RAX
store i64 %RBP_11, i64* %RBP
store i64 4195942, i64* %RIP
%555 = bitcast i128 %XMM0_13 to <4 x float>
store i128 %XMM0_13, <4 x float>* %XMM0
%556 = bitcast i128 %XMM1_22 to <4 x float>
store i128 %XMM1_22, <4 x float>* %XMM1
%557 = bitcast i256 %YMM0_13 to <8 x float>
store i256 %YMM0_13, <8 x float>* %YMM0
%558 = bitcast i256 %YMM1_22 to <8 x float>
store i256 %YMM1_22, <8 x float>* %YMM1
%559 = bitcast i512 %ZMM0_13 to <16 x float>
store i512 %ZMM0_13, <16 x float>* %ZMM0
%560 = bitcast i512 %ZMM1_22 to <16 x float>
store i512 %ZMM1_22, <16 x float>* %ZMM1
br label %bb_400666
bb_4006AB:                                        ; preds = %bb_400666
%RIP_98 = 4196016
%EIP_84 = 4196016
%RBP_12 = %RBP
%562 = %RBP-64
%563 = load double, double* %562, align 1
%564 = bitcast double %563 to i64
%ZMM0_14 = %ZMM0
%565 = %ZMM0
%XMM0_14 = %ZMM0
%566 = zext double %563 to i128
%567 = and i128 %ZMM0, -18446744073709551616
%XMM0_15 = or i128 %566, %567
%568 = %ZMM0
%YMM0_14 = %ZMM0
%569 = zext i128 %XMM0_15 to i256
%570 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_15 = or i256 %569, %570
%571 = %ZMM0
%572 = zext i128 %XMM0_15 to i512
%573 = and i512 %571, -340282366920938463463374607431768211456
%ZMM0_15 = or i512 %572, %573
%RIP_99 = 4196021
%EIP_85 = 4196021
%RSP_9 = %RSP
%574 = %RSP-8
store i64 4196021, i64* %574
%ESP_6 = %RSP-8
store i32 %EIP_85, i32* %EIP
store i32 %ESP_6, i32* %ESP
store i64 %RBP_12, i64* %RBP
store i64 %RIP_99, i64* %RIP
store i64 %RSP_10, i64* %RSP
%575 = bitcast i128 %XMM0_15 to <4 x float>
store i128 %XMM0_15, <4 x float>* %XMM0
%576 = bitcast i256 %YMM0_15 to <8 x float>
store i256 %YMM0_15, <8 x float>* %YMM0
%577 = bitcast i512 %ZMM0_15 to <16 x float>
store i512 %ZMM0_15, <16 x float>* %ZMM0
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
%RIP_100 = %RIP
%RIP_101 = %RIP+4
%EIP_86 = %RIP+4
%ZMM0_16 = %ZMM0
%598 = %ZMM0
%XMM0_16 = %ZMM0
%599 = %ZMM0
%600 = bitcast i64 %599 to double
%EAX_11 = %ZMM0
%RAX_15 = %RAX
%RAX_16 = %ZMM0
%601 = lshr i32 %ZMM0, 8
%RIP_102 = %RIP+7
%EIP_87 = %RIP+7
%RBP_13 = %RBP
%603 = %RBP-4
store i32 %EAX_11, i32* %603, align 1
store i32 %EAX_11, i32* %EAX
store i32 %EIP_87, i32* %EIP
store i64 %RAX_16, i64* %RAX
store i64 %RBP_13, i64* %RBP
store i64 %RIP_102, i64* %RIP
%604 = %ZMM0
store <4 x float> %604, <4 x float>* %XMM0
store <16 x float> %ZMM0_16, <16 x float>* %ZMM0
br label %bb_4006BC
bb_4006BC:                                        ; preds = %bb_4006AB, %bb_400654, %bb_400623, %bb_4005EA
%RIP_44 = 4196031
%EIP_40 = 4196031
%RBP_3 = %RBP
%606 = %RBP-4
%EAX_3 = [ %RBP -4 ]
%RAX_4 = %RAX
%RAX_5 = %RBP-4
%607 = lshr i32 [ %RBP -4 ], 8
%RIP_45 = 4196035
%EIP_41 = 4196035
%RSP_3 = %RSP
%RSP_4 = %RSP+80
%ESP_2 = %RSP+80
%RIP_46 = 4196036
%EIP_42 = 4196036
%RSP_5 = %RSP+88
%ESP_3 = %RSP+88
%609 = %RSP+88-8
%RBP_4 = [ %RSP +96 ]
%EBP_1 = %RSP+96
%RIP_47 = 4196037
%EIP_43 = 4196037
%610 = %RSP+88+8
%RIP_48 = [ %RSP +88 ]
%ESP_4 = %RSP+96
%EIP_44 = %RSP+88
%ZF_04 = icmp eq i64 %RSP_4, 0
%SF_0 = icmp slt i64 %RSP+80, 0
%611 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP, i64 80)
%OF_0 = extractvalue { i64, i1 } %611, 1
%612 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP, i64 80)
%CF_05 = extractvalue { i64, i1 } %612, 1
%613 = %RSP+80
%614 = call i8 @llvm.ctpop.i8(i8 %613)
%615 = trunc i8 %614 to i1
%PF_06 = icmp eq i8 %614, false
%616 = zext i1 %CF_05 to i32
%617 = shl i1 %CF_05, 0
%619 = zext i1 %PF_06 to i32
%620 = shl i1 %PF_06, 2
%622 = zext i1 false to i32
%623 = shl i1 false, 4
%625 = zext i1 %ZF_04 to i32
%626 = shl i1 %ZF_04, 6
%628 = zext i1 %SF_0 to i32
%629 = shl i1 %SF_0, 7
%631 = zext i1 %OF_0 to i32
%632 = shl i1 %OF_0, 11
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

