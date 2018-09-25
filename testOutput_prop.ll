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
%2 = %1
%XMM0_init = %regset*
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%3 = %regset*
%4 = %3
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
%7 = %6
%XMM1_init = %regset*
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %regset*
%9 = %8
%YMM1_init = %regset*
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%ZMM2_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 87
%ZMM2_init = %regset*
%ZMM2 = alloca <16 x float>
store <16 x float> %ZMM2_init, <16 x float>* %ZMM2
%10 = %regset*
%11 = %10
%XMM2_init = %regset*
%XMM2 = alloca <4 x float>
store <4 x float> %XMM2_init, <4 x float>* %XMM2
%12 = %regset*
%13 = %12
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
%28 = %27
%29 = %ZMM0
%30 = %29
%31 = xor <2 x i64> %27, %29
%XMM0_1 = %31
%32 = %ZMM0
%YMM0_0 = %ZMM0
%33 = %31
%34 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_1 = %YMM0_0 : %XMM0_1
%35 = %ZMM0
%36 = %31
%37 = and i512 %35, -340282366920938463463374607431768211456
%ZMM0_1 = %35 : %XMM0_1
%RIP_5 = 4195685
%EIP_4 = 4195685
%RIP_6 = 4195690
%EIP_5 = 4195690
%38 = 4003244811
%39 = 4003244811
%ZMM1_0 = %ZMM1
%40 = %ZMM1
%XMM1_0 = %ZMM1
%41 = 4003244811
%42 = and i128 %ZMM1, -18446744073709551616
%XMM1_1 = %XMM1_0 : %39
%43 = %ZMM1
%YMM1_0 = %ZMM1
%44 = %XMM1_0
%45 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_1 = %YMM1_0 : %XMM1_1
%46 = %ZMM1
%47 = %XMM1_0
%48 = and i512 %46, -340282366920938463463374607431768211456
%ZMM1_1 = %46 : %XMM1_1
%RIP_7 = 4195700
%EIP_6 = 4195700
%RIP_8 = 4195705
%EIP_7 = 4195705
%49 = 1336630430
%50 = 1336630430
%ZMM2_0 = %ZMM2
%51 = %ZMM2
%XMM2_0 = %ZMM2
%52 = 1336630430
%53 = and i128 %ZMM2, -18446744073709551616
%XMM2_1 = %XMM2_0 : %50
%54 = %ZMM2
%YMM2_0 = %ZMM2
%55 = %XMM2_0
%56 = and i256 %ZMM2, -340282366920938463463374607431768211456
%YMM2_1 = %YMM2_0 : %XMM2_1
%57 = %ZMM2
%58 = %XMM2_0
%59 = and i512 %57, -340282366920938463463374607431768211456
%ZMM2_1 = %57 : %XMM2_1
%RIP_9 = 4195712
%EIP_8 = 4195712
%61 = %RSP-8-4
store i32 0, i32* %61, align 1
%RIP_10 = 4195717
%EIP_9 = 4195717
%62 = %XMM2_0
%63 = %50
%65 = %RSP-8-24
store double %50, double* %65, align 1
%RIP_11 = 4195722
%EIP_10 = 4195722
%66 = %XMM1_0
%67 = %39
%69 = %RSP-8-16
store double %39, double* %69, align 1
%RIP_12 = 4195727
%EIP_11 = 4195727
%71 = %RSP-8-16
%72 = load double, double* %71, align 1
%73 = %72
%74 = %72
%75 = and i128 %XMM1_0 : %39, -18446744073709551616
%XMM1_2 = %XMM1_1 : %73
%76 = %XMM1_1
%77 = and i256 %YMM1_0 : %XMM1_1, -340282366920938463463374607431768211456
%YMM1_2 = %YMM1_1 : %XMM1_2
%78 = %XMM1_1
%79 = and i512 %46 : %XMM1_1, -340282366920938463463374607431768211456
%ZMM1_2 = %ZMM1_1 : %XMM1_2
%RIP_13 = 4195732
%EIP_12 = 4195732
%80 = %XMM1_1
%81 = %73
%83 = %RSP-8-24
%84 = load double, double* %83, align 1
%85 = fadd double %73, %84
%86 = %85
%87 = %85
%88 = and i128 %XMM1_1 : %73, -18446744073709551616
%XMM1_3 = %XMM1_2 : %86
%89 = %XMM1_2
%90 = and i256 %YMM1_1 : %XMM1_2, -340282366920938463463374607431768211456
%YMM1_3 = %YMM1_2 : %XMM1_3
%91 = %XMM1_2
%92 = and i512 %ZMM1_1 : %XMM1_2, -340282366920938463463374607431768211456
%ZMM1_3 = %ZMM1_2 : %XMM1_3
%RIP_14 = 4195737
%EIP_13 = 4195737
%93 = %XMM1_2
%94 = %86
%96 = %RSP-8-32
store double %86, double* %96, align 1
%RIP_15 = 4195742
%EIP_14 = 4195742
%98 = %RSP-8-16
%99 = load double, double* %98, align 1
%100 = %99
%101 = %99
%102 = and i128 %XMM1_2 : %86, -18446744073709551616
%XMM1_4 = %XMM1_3 : %100
%103 = %XMM1_3
%104 = and i256 %YMM1_2 : %XMM1_3, -340282366920938463463374607431768211456
%YMM1_4 = %YMM1_3 : %XMM1_4
%105 = %XMM1_3
%106 = and i512 %ZMM1_2 : %XMM1_3, -340282366920938463463374607431768211456
%ZMM1_4 = %ZMM1_3 : %XMM1_4
%RIP_16 = 4195747
%EIP_15 = 4195747
%107 = %XMM1_3
%108 = %100
%110 = %RSP-8-24
%111 = load double, double* %110, align 1
%112 = fsub double %100, %111
%113 = %112
%114 = %112
%115 = and i128 %XMM1_3 : %100, -18446744073709551616
%XMM1_5 = %XMM1_4 : %113
%116 = %XMM1_4
%117 = and i256 %YMM1_3 : %XMM1_4, -340282366920938463463374607431768211456
%YMM1_5 = %YMM1_4 : %XMM1_5
%118 = %XMM1_4
%119 = and i512 %ZMM1_3 : %XMM1_4, -340282366920938463463374607431768211456
%ZMM1_5 = %ZMM1_4 : %XMM1_5
%RIP_17 = 4195752
%EIP_16 = 4195752
%120 = %XMM1_4
%121 = %113
%123 = %RSP-8-40
store double %113, double* %123, align 1
%RIP_18 = 4195757
%EIP_17 = 4195757
%125 = %RSP-8-16
%126 = load double, double* %125, align 1
%127 = %126
%128 = %126
%129 = and i128 %XMM1_4 : %113, -18446744073709551616
%XMM1_6 = %XMM1_5 : %127
%130 = %XMM1_5
%131 = and i256 %YMM1_4 : %XMM1_5, -340282366920938463463374607431768211456
%YMM1_6 = %YMM1_5 : %XMM1_6
%132 = %XMM1_5
%133 = and i512 %ZMM1_4 : %XMM1_5, -340282366920938463463374607431768211456
%ZMM1_6 = %ZMM1_5 : %XMM1_6
%RIP_19 = 4195762
%EIP_18 = 4195762
%134 = %XMM1_5
%135 = %127
%137 = %RSP-8-24
%138 = load double, double* %137, align 1
%139 = fmul double %127, %138
%140 = %139
%141 = %139
%142 = and i128 %XMM1_5 : %127, -18446744073709551616
%XMM1_7 = %XMM1_6 : %140
%143 = %XMM1_6
%144 = and i256 %YMM1_5 : %XMM1_6, -340282366920938463463374607431768211456
%YMM1_7 = %YMM1_6 : %XMM1_7
%145 = %XMM1_6
%146 = and i512 %ZMM1_5 : %XMM1_6, -340282366920938463463374607431768211456
%ZMM1_7 = %ZMM1_6 : %XMM1_7
%RIP_20 = 4195767
%EIP_19 = 4195767
%147 = %XMM1_6
%148 = %140
%150 = %RSP-8-48
store double %140, double* %150, align 1
%RIP_21 = 4195772
%EIP_20 = 4195772
%152 = %RSP-8-16
%153 = load double, double* %152, align 1
%154 = %153
%155 = %153
%156 = and i128 %XMM1_6 : %140, -18446744073709551616
%XMM1_8 = %XMM1_7 : %154
%157 = %XMM1_7
%158 = and i256 %YMM1_6 : %XMM1_7, -340282366920938463463374607431768211456
%YMM1_8 = %YMM1_7 : %XMM1_8
%159 = %XMM1_7
%160 = and i512 %ZMM1_6 : %XMM1_7, -340282366920938463463374607431768211456
%ZMM1_8 = %ZMM1_7 : %XMM1_8
%RIP_22 = 4195777
%EIP_21 = 4195777
%161 = %XMM1_7
%162 = %154
%164 = %RSP-8-24
%165 = load double, double* %164, align 1
%166 = fdiv double %154, %165
%167 = %166
%168 = %166
%169 = and i128 %XMM1_7 : %154, -18446744073709551616
%XMM1_9 = %XMM1_8 : %167
%170 = %XMM1_8
%171 = and i256 %YMM1_7 : %XMM1_8, -340282366920938463463374607431768211456
%YMM1_9 = %YMM1_8 : %XMM1_9
%172 = %XMM1_8
%173 = and i512 %ZMM1_7 : %XMM1_8, -340282366920938463463374607431768211456
%ZMM1_9 = %ZMM1_8 : %XMM1_9
%RIP_23 = 4195782
%EIP_22 = 4195782
%174 = %XMM1_8
%175 = %167
%177 = %RSP-8-56
store double %167, double* %177, align 1
%RIP_24 = 4195787
%EIP_23 = 4195787
%179 = %RSP-8-32
%180 = load double, double* %179, align 1
%181 = %180
%182 = %180
%183 = and i128 %XMM1_8 : %167, -18446744073709551616
%XMM1_10 = %XMM1_9 : %181
%184 = %XMM1_9
%185 = and i256 %YMM1_8 : %XMM1_9, -340282366920938463463374607431768211456
%YMM1_10 = %YMM1_9 : %XMM1_10
%186 = %XMM1_9
%187 = and i512 %ZMM1_8 : %XMM1_9, -340282366920938463463374607431768211456
%ZMM1_10 = %ZMM1_9 : %XMM1_10
%RIP_25 = 4195792
%EIP_24 = 4195792
%188 = %XMM1_9
%189 = %181
%191 = %RSP-8-40
%192 = load double, double* %191, align 1
%193 = fmul double %181, %192
%194 = %193
%195 = %193
%196 = and i128 %XMM1_9 : %181, -18446744073709551616
%XMM1_11 = %XMM1_10 : %194
%197 = %XMM1_10
%198 = and i256 %YMM1_9 : %XMM1_10, -340282366920938463463374607431768211456
%YMM1_11 = %YMM1_10 : %XMM1_11
%199 = %XMM1_10
%200 = and i512 %ZMM1_9 : %XMM1_10, -340282366920938463463374607431768211456
%ZMM1_11 = %ZMM1_10 : %XMM1_11
%RIP_26 = 4195797
%EIP_25 = 4195797
%201 = %XMM1_10
%202 = %194
%204 = %RSP-8-48
%205 = load double, double* %204, align 1
%206 = fdiv double %194, %205
%207 = %206
%208 = %206
%209 = and i128 %XMM1_10 : %194, -18446744073709551616
%XMM1_12 = %XMM1_11 : %207
%210 = %XMM1_11
%211 = and i256 %YMM1_10 : %XMM1_11, -340282366920938463463374607431768211456
%YMM1_12 = %YMM1_11 : %XMM1_12
%212 = %XMM1_11
%213 = and i512 %ZMM1_10 : %XMM1_11, -340282366920938463463374607431768211456
%ZMM1_12 = %ZMM1_11 : %XMM1_12
%RIP_27 = 4195802
%EIP_26 = 4195802
%214 = %XMM1_11
%215 = %207
%217 = %RSP-8-56
%218 = load double, double* %217, align 1
%219 = fadd double %207, %218
%220 = %219
%221 = %219
%222 = and i128 %XMM1_11 : %207, -18446744073709551616
%XMM1_13 = %XMM1_12 : %220
%223 = %XMM1_12
%224 = and i256 %YMM1_11 : %XMM1_12, -340282366920938463463374607431768211456
%YMM1_13 = %YMM1_12 : %XMM1_13
%225 = %XMM1_12
%226 = and i512 %ZMM1_11 : %XMM1_12, -340282366920938463463374607431768211456
%ZMM1_13 = %ZMM1_12 : %XMM1_13
%RIP_28 = 4195807
%EIP_27 = 4195807
%227 = %XMM1_12
%228 = %220
%230 = %RSP-8-64
store double %220, double* %230, align 1
%RIP_29 = 4195812
%EIP_28 = 4195812
%231 = %31
%232 = %231
%234 = %RSP-8-64
%235 = load double, double* %234, align 1
%ZF_0 = fcmp ueq double %232, %235
%PF_0 = fcmp uno double %231, %235
%CF_0 = fcmp ult double %231, %235
%236 = %CF_0
%237 = shl i32 %CF_0, 0
%239 = %PF_0
%240 = shl i32 %PF_0, 2
%242 = false
%243 = shl i32 false, 4
%245 = %ZF_0
%246 = shl i32 %ZF_0, 6
%248 = false
%249 = shl i32 false, 7
%251 = false
%252 = shl i32 false, 11
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
%254 = %XMM1_12
store <4 x float> %220, <4 x float>* %XMM1
%255 = %XMM2_0
store <4 x float> %50, <4 x float>* %XMM2
%256 = %YMM0_0
store <8 x float> %XMM0_1, <8 x float>* %YMM0
%257 = %YMM1_12
store <8 x float> %XMM1_13, <8 x float>* %YMM1
%258 = %YMM2_0
store <8 x float> %XMM2_1, <8 x float>* %YMM2
%259 = %35
store <16 x float> %XMM0_1, <16 x float>* %ZMM0
%260 = %ZMM1_12
store <16 x float> %XMM1_13, <16 x float>* %ZMM1
%261 = %57
store <16 x float> %XMM2_1, <16 x float>* %ZMM2
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
%270 = %269
%271 = %ZMM0
%272 = %271
%273 = xor <2 x i64> %269, %271
%XMM0_3 = %273
%274 = %ZMM0
%YMM0_2 = %ZMM0
%275 = %273
%276 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_3 = %YMM0_2 : %XMM0_3
%277 = %ZMM0
%278 = %273
%279 = and i512 %277, -340282366920938463463374607431768211456
%ZMM0_3 = %277 : %XMM0_3
%RIP_39 = 4195839
%EIP_36 = 4195839
%RBP_2 = %RBP
%281 = %RBP-64
%282 = load double, double* %281, align 1
%283 = %282
%ZMM1_14 = %ZMM1
%284 = %ZMM1
%XMM1_14 = %ZMM1
%285 = %282
%286 = and i128 %ZMM1, -18446744073709551616
%XMM1_15 = %XMM1_14 : %283
%287 = %ZMM1
%YMM1_14 = %ZMM1
%288 = %XMM1_14
%289 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_15 = %YMM1_14 : %XMM1_15
%290 = %ZMM1
%291 = %XMM1_14
%292 = and i512 %290, -340282366920938463463374607431768211456
%ZMM1_15 = %290 : %XMM1_15
%RIP_40 = 4195843
%EIP_37 = 4195843
%293 = %XMM1_14
%294 = %283
%295 = %273
%296 = %295
%ZF_01 = fcmp ueq double %294, %296
%PF_02 = fcmp uno double %283, %295
%CF_03 = fcmp ult double %283, %295
%297 = %CF_03
%298 = shl i32 %CF_03, 0
%300 = %PF_02
%301 = shl i32 %PF_02, 2
%303 = false
%304 = shl i32 false, 4
%306 = %ZF_01
%307 = shl i32 %ZF_01, 6
%309 = false
%310 = shl i32 false, 7
%312 = false
%313 = shl i32 false, 11
%RIP_41 = 4195849
%EIP_38 = 4195849
store i32 4195888, i32* %EIP
store i64 %RBP_2, i64* %RBP
store i64 4195888, i64* %RIP
%314 = %273
store <4 x float> %314, <4 x float>* %XMM0
%315 = %XMM1_14
store <4 x float> %283, <4 x float>* %XMM1
%316 = %YMM0_2
store <8 x float> %XMM0_3, <8 x float>* %YMM0
%317 = %YMM1_14
store <8 x float> %XMM1_15, <8 x float>* %YMM1
%318 = %277
store <16 x float> %XMM0_3, <16 x float>* %ZMM0
%319 = %290
store <16 x float> %XMM1_15, <16 x float>* %ZMM1
br i1 %CF_03, label %bb_400630, label %bb_400609
bb_400609:                                        ; preds = %bb_4005F7
%RIP_50 = 4195859
%EIP_45 = 4195859
%RIP_51 = 4195864
%EIP_46 = 4195864
%320 = 5
%321 = 5
%ZMM0_4 = %ZMM0
%322 = %ZMM0
%XMM0_4 = %ZMM0
%323 = 5
%324 = and i128 %ZMM0, -18446744073709551616
%XMM0_5 = %XMM0_4 : %321
%325 = %ZMM0
%YMM0_4 = %ZMM0
%326 = %XMM0_4
%327 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_5 = %YMM0_4 : %XMM0_5
%328 = %ZMM0
%329 = %XMM0_4
%330 = and i512 %328, -340282366920938463463374607431768211456
%ZMM0_5 = %328 : %XMM0_5
%RIP_52 = 4195869
%EIP_47 = 4195869
%331 = %XMM0_4
%332 = %321
%RBP_5 = %RBP
%334 = %RBP-64
%335 = load double, double* %334, align 1
%ZF_07 = fcmp ueq double %332, %335
%PF_08 = fcmp uno double %321, %335
%CF_09 = fcmp ult double %321, %335
%336 = %CF_09
%337 = shl i32 %CF_09, 0
%339 = %PF_08
%340 = shl i32 %PF_08, 2
%342 = false
%343 = shl i32 false, 4
%345 = %ZF_07
%346 = shl i32 %ZF_07, 6
%348 = false
%349 = shl i32 false, 7
%351 = false
%352 = shl i32 false, 11
%RIP_53 = 4195875
%EIP_48 = 4195875
%CC_BE_010 = or i1 %CF_09, %ZF_07
store i32 5, i32* %EAX
store i32 4195888, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 4195888, i64* %RIP
%353 = %XMM0_4
store <4 x float> %321, <4 x float>* %XMM0
%354 = %YMM0_4
store <8 x float> %XMM0_5, <8 x float>* %YMM0
%355 = %328
store <16 x float> %XMM0_5, <16 x float>* %ZMM0
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
%362 = 5
%363 = 5
%ZMM0_6 = %ZMM0
%364 = %ZMM0
%XMM0_6 = %ZMM0
%365 = 5
%366 = and i128 %ZMM0, -18446744073709551616
%XMM0_7 = %XMM0_6 : %363
%367 = %ZMM0
%YMM0_6 = %ZMM0
%368 = %XMM0_6
%369 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_7 = %YMM0_6 : %XMM0_7
%370 = %ZMM0
%371 = %XMM0_6
%372 = and i512 %370, -340282366920938463463374607431768211456
%ZMM0_7 = %370 : %XMM0_7
%RIP_58 = 4195908
%EIP_52 = 4195908
%RBP_6 = %RBP
%374 = %RBP-64
%375 = load double, double* %374, align 1
%376 = %375
%ZMM1_16 = %ZMM1
%377 = %ZMM1
%XMM1_16 = %ZMM1
%378 = %375
%379 = and i128 %ZMM1, -18446744073709551616
%XMM1_17 = %XMM1_16 : %376
%380 = %ZMM1
%YMM1_16 = %ZMM1
%381 = %XMM1_16
%382 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_17 = %YMM1_16 : %XMM1_17
%383 = %ZMM1
%384 = %XMM1_16
%385 = and i512 %383, -340282366920938463463374607431768211456
%ZMM1_17 = %383 : %XMM1_17
%RIP_59 = 4195912
%EIP_53 = 4195912
%386 = %XMM1_16
%387 = %376
%388 = %XMM0_6
%389 = %363
%ZF_011 = fcmp ueq double %387, %389
%PF_012 = fcmp uno double %376, %363
%CF_013 = fcmp ult double %376, %363
%390 = %CF_013
%391 = shl i32 %CF_013, 0
%393 = %PF_012
%394 = shl i32 %PF_012, 2
%396 = false
%397 = shl i32 false, 4
%399 = %ZF_011
%400 = shl i32 %ZF_011, 6
%402 = false
%403 = shl i32 false, 7
%405 = false
%406 = shl i32 false, 11
%RIP_60 = 4195918
%EIP_54 = 4195918
%CC_NE_0 = xor i1 %ZF_011, true
store i32 5, i32* %EAX
store i32 4195937, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195937, i64* %RIP
%407 = %XMM0_6
store <4 x float> %363, <4 x float>* %XMM0
%408 = %XMM1_16
store <4 x float> %376, <4 x float>* %XMM1
%409 = %YMM0_6
store <8 x float> %XMM0_7, <8 x float>* %YMM0
%410 = %YMM1_16
store <8 x float> %XMM1_17, <8 x float>* %YMM1
%411 = %370
store <16 x float> %XMM0_7, <16 x float>* %ZMM0
%412 = %383
store <16 x float> %XMM1_17, <16 x float>* %ZMM1
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
%422 = %421
%423 = %ZMM0
%424 = %423
%425 = xor <2 x i64> %421, %423
%XMM0_9 = %425
%426 = %ZMM0
%YMM0_8 = %ZMM0
%427 = %425
%428 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_9 = %YMM0_8 : %XMM0_9
%429 = %ZMM0
%430 = %425
%431 = and i512 %429, -340282366920938463463374607431768211456
%ZMM0_9 = %429 : %XMM0_9
%RIP_80 = 4195950
%EIP_69 = 4195950
%RBP_9 = %RBP
%433 = %RBP-64
%434 = load double, double* %433, align 1
%435 = %434
%ZMM1_18 = %ZMM1
%436 = %ZMM1
%XMM1_18 = %ZMM1
%437 = %434
%438 = and i128 %ZMM1, -18446744073709551616
%XMM1_19 = %XMM1_18 : %435
%439 = %ZMM1
%YMM1_18 = %ZMM1
%440 = %XMM1_18
%441 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_19 = %YMM1_18 : %XMM1_19
%442 = %ZMM1
%443 = %XMM1_18
%444 = and i512 %442, -340282366920938463463374607431768211456
%ZMM1_19 = %442 : %XMM1_19
%RIP_81 = 4195954
%EIP_70 = 4195954
%445 = %XMM1_18
%446 = %435
%447 = %425
%448 = %447
%ZF_015 = fcmp ueq double %446, %448
%PF_016 = fcmp uno double %435, %447
%CF_017 = fcmp ult double %435, %447
%449 = %CF_017
%450 = shl i32 %CF_017, 0
%452 = %PF_016
%453 = shl i32 %PF_016, 2
%455 = false
%456 = shl i32 false, 4
%458 = %ZF_015
%459 = shl i32 %ZF_015, 6
%461 = false
%462 = shl i32 false, 7
%464 = false
%465 = shl i32 false, 11
%RIP_82 = 4195960
%EIP_71 = 4195960
%CC_BE_018 = or i1 %CF_017, %ZF_015
store i32 4196011, i32* %EIP
store i64 %RBP_9, i64* %RBP
store i64 4196011, i64* %RIP
%466 = %425
store <4 x float> %466, <4 x float>* %XMM0
%467 = %XMM1_18
store <4 x float> %435, <4 x float>* %XMM1
%468 = %YMM0_8
store <8 x float> %XMM0_9, <8 x float>* %YMM0
%469 = %YMM1_18
store <8 x float> %XMM1_19, <8 x float>* %YMM1
%470 = %429
store <16 x float> %XMM0_9, <16 x float>* %ZMM0
%471 = %442
store <16 x float> %XMM1_19, <16 x float>* %ZMM1
br i1 %CC_BE_018, label %bb_4006AB, label %bb_400678
bb_400678:                                        ; preds = %bb_400666
%RIP_85 = 4195968
%EIP_73 = 4195968
%RIP_86 = 4195973
%EIP_74 = 4195973
%RBP_10 = %RBP
%473 = %RBP-64
%474 = load double, double* %473, align 1
%475 = %474
%ZMM0_10 = %ZMM0
%476 = %ZMM0
%XMM0_10 = %ZMM0
%477 = %474
%478 = and i128 %ZMM0, -18446744073709551616
%XMM0_11 = %XMM0_10 : %475
%479 = %ZMM0
%YMM0_10 = %ZMM0
%480 = %XMM0_10
%481 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_11 = %YMM0_10 : %XMM0_11
%482 = %ZMM0
%483 = %XMM0_10
%484 = and i512 %482, -340282366920938463463374607431768211456
%ZMM0_11 = %482 : %XMM0_11
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
%489 = %XMM0_10
store <4 x float> %475, <4 x float>* %XMM0
%490 = %YMM0_10
store <8 x float> %XMM0_11, <8 x float>* %YMM0
%491 = %482
store <16 x float> %XMM0_11, <16 x float>* %ZMM0
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
%512 = 4196184
%513 = [ 4196184 ]
%514 = %513
%ZMM0_12 = %ZMM0
%515 = %ZMM0
%XMM0_12 = %ZMM0
%516 = %513
%517 = and i128 %ZMM0, -18446744073709551616
%XMM0_13 = %XMM0_12 : %514
%518 = %ZMM0
%YMM0_12 = %ZMM0
%519 = %XMM0_12
%520 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_13 = %YMM0_12 : %XMM0_13
%521 = %ZMM0
%522 = %XMM0_12
%523 = and i512 %521, -340282366920938463463374607431768211456
%ZMM0_13 = %521 : %XMM0_13
%RIP_91 = %RIP+14
%EIP_78 = %RIP+14
%RBP_11 = %RBP
%525 = %RBP-64
%526 = load double, double* %525, align 1
%527 = %526
%ZMM1_20 = %ZMM1
%528 = %ZMM1
%XMM1_20 = %ZMM1
%529 = %526
%530 = and i128 %ZMM1, -18446744073709551616
%XMM1_21 = %XMM1_20 : %527
%531 = %ZMM1
%YMM1_20 = %ZMM1
%532 = %XMM1_20
%533 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_21 = %YMM1_20 : %XMM1_21
%534 = %ZMM1
%535 = %XMM1_20
%536 = and i512 %534, -340282366920938463463374607431768211456
%ZMM1_21 = %534 : %XMM1_21
%RIP_92 = %RIP+18
%EIP_79 = %RIP+18
%537 = %XMM1_20
%538 = %527
%539 = %XMM0_12
%540 = %514
%541 = fsub double %527, %514
%542 = %541
%543 = %541
%544 = and i128 %XMM1_20 : %527, -18446744073709551616
%XMM1_22 = %XMM1_21 : %542
%545 = %XMM1_21
%546 = and i256 %YMM1_20 : %XMM1_21, -340282366920938463463374607431768211456
%YMM1_22 = %YMM1_21 : %XMM1_22
%547 = %XMM1_21
%548 = and i512 %534 : %XMM1_21, -340282366920938463463374607431768211456
%ZMM1_22 = %ZMM1_21 : %XMM1_22
%RIP_93 = %RIP+23
%EIP_80 = %RIP+23
%549 = %XMM1_21
%550 = %542
%552 = %RBP-64
store double %542, double* %552, align 1
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
%555 = %XMM0_12
store <4 x float> %514, <4 x float>* %XMM0
%556 = %XMM1_21
store <4 x float> %542, <4 x float>* %XMM1
%557 = %YMM0_12
store <8 x float> %XMM0_13, <8 x float>* %YMM0
%558 = %YMM1_21
store <8 x float> %XMM1_22, <8 x float>* %YMM1
%559 = %521
store <16 x float> %XMM0_13, <16 x float>* %ZMM0
%560 = %ZMM1_21
store <16 x float> %XMM1_22, <16 x float>* %ZMM1
br label %bb_400666
bb_4006AB:                                        ; preds = %bb_400666
%RIP_98 = 4196016
%EIP_84 = 4196016
%RBP_12 = %RBP
%562 = %RBP-64
%563 = load double, double* %562, align 1
%564 = %563
%ZMM0_14 = %ZMM0
%565 = %ZMM0
%XMM0_14 = %ZMM0
%566 = %563
%567 = and i128 %ZMM0, -18446744073709551616
%XMM0_15 = %XMM0_14 : %564
%568 = %ZMM0
%YMM0_14 = %ZMM0
%569 = %XMM0_14
%570 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_15 = %YMM0_14 : %XMM0_15
%571 = %ZMM0
%572 = %XMM0_14
%573 = and i512 %571, -340282366920938463463374607431768211456
%ZMM0_15 = %571 : %XMM0_15
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
%575 = %XMM0_14
store <4 x float> %564, <4 x float>* %XMM0
%576 = %YMM0_14
store <8 x float> %XMM0_15, <8 x float>* %YMM0
%577 = %571
store <16 x float> %XMM0_15, <16 x float>* %ZMM0
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
%600 = %599
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
%615 = %614
%PF_06 = icmp eq i1 %614, false
%616 = %CF_05
%617 = shl i32 %CF_05, 0
%619 = %PF_06
%620 = shl i32 %PF_06, 2
%622 = false
%623 = shl i32 false, 4
%625 = %ZF_04
%626 = shl i32 %ZF_04, 6
%628 = %SF_0
%629 = shl i32 %SF_0, 7
%631 = %OF_0
%632 = shl i32 %OF_0, 11
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

