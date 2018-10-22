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
%1 = %ZMM0_init
%2 = %ZMM0_init
%XMM0_init = bitcast i128 %ZMM0_init to <4 x float>
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%3 = %ZMM0_init
%4 = %ZMM0_init
%YMM0_init = bitcast i256 %ZMM0_init to <8 x float>
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
%6 = %ZMM1_init
%7 = %ZMM1_init
%XMM1_init = bitcast i128 %ZMM1_init to <4 x float>
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %ZMM1_init
%9 = %ZMM1_init
%YMM1_init = bitcast i256 %ZMM1_init to <8 x float>
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%ZMM2_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 87
%ZMM2_init = load <16 x float>, <16 x float>* %ZMM2_ptr
%ZMM2 = alloca <16 x float>
store <16 x float> %ZMM2_init, <16 x float>* %ZMM2
%10 = %ZMM2_init
%11 = %ZMM2_init
%XMM2_init = bitcast i128 %ZMM2_init to <4 x float>
%XMM2 = alloca <4 x float>
store <4 x float> %XMM2_init, <4 x float>* %XMM2
%12 = %ZMM2_init
%13 = %ZMM2_init
%YMM2_init = bitcast i256 %ZMM2_init to <8 x float>
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
%16 = [%RAX]
store i64 %16, i64* %RAX_ptr
%17 = [%RBP]
store i64 %17, i64* %RBP_ptr
%18 = [%RDI]
store i64 %18, i64* %RDI_ptr
%19 = [%RIP]
store i64 %19, i64* %RIP_ptr
%20 = [%RSP]
store i64 %20, i64* %RSP_ptr
%21 = [%ZMM0]
store <16 x float> %21, <16 x float>* %ZMM0_ptr
%22 = [%ZMM1]
store <16 x float> %22, <16 x float>* %ZMM1_ptr
%23 = [%ZMM2]
store <16 x float> %23, <16 x float>* %ZMM2_ptr
ret void
bb_400550:                                        ; preds = %entry_fn_400550
%RIP_1 = 4195665
%EIP_0 = 4195665
%RBP_0 = %RBP
%RSP_0 = %RSP
%25 = %RSP-8
store i64 %RBP, i64* %RSP-8, align 1
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
%28 = %ZMM0
%29 = %ZMM0
%30 = %ZMM0
%31 = xor <2 x i64> %ZMM0, %ZMM0
%XMM0_1 = %31
%32 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %31
%35 = %ZMM0
%ZMM0_1 = %31
%RIP_5 = 4195685
%EIP_4 = 4195685
%RIP_6 = 4195690
%EIP_5 = 4195690
%38 = 4003244811
%39 = 4003244811
%ZMM1_0 = %ZMM1
%40 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = 4003244811
%43 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = 4003244811
%46 = %ZMM1
%ZMM1_1 = 4003244811
%RIP_7 = 4195700
%EIP_6 = 4195700
%RIP_8 = 4195705
%EIP_7 = 4195705
%49 = 1336630430
%50 = 1336630430
%ZMM2_0 = %ZMM2
%51 = %ZMM2
%XMM2_0 = %ZMM2
%XMM2_1 = 1336630430
%54 = %ZMM2
%YMM2_0 = %ZMM2
%YMM2_1 = 1336630430
%57 = %ZMM2
%ZMM2_1 = 1336630430
%RIP_9 = 4195712
%EIP_8 = 4195712
%61 = %RSP-12
store i32 0, i32* %RSP-12, align 1
%RIP_10 = 4195717
%EIP_9 = 4195717
%62 = 1336630430
%63 = 1336630430
%65 = %RSP-32
store double 1336630430, double* %RSP-32, align 1
%RIP_11 = 4195722
%EIP_10 = 4195722
%66 = 4003244811
%67 = 4003244811
%69 = %RSP-24
store double 4003244811, double* %RSP-24, align 1
%RIP_12 = 4195727
%EIP_11 = 4195727
%71 = %RSP-24
%72 = [%RSP-24]
%73 = %72
%XMM1_2 = %72
%YMM1_2 = %72
%ZMM1_2 = %72
%RIP_13 = 4195732
%EIP_12 = 4195732
%80 = %72
%81 = %72
%83 = %RSP-32
%84 = [%RSP-32]
%85 = fadd double %72, %84
%86 = %85
%XMM1_3 = %85
%YMM1_3 = %85
%ZMM1_3 = %85
%RIP_14 = 4195737
%EIP_13 = 4195737
%93 = %85
%94 = %85
%96 = %RSP-40
store double %85, double* %RSP-40, align 1
%RIP_15 = 4195742
%EIP_14 = 4195742
%98 = %RSP-24
%99 = [%RSP-24]
%100 = %99
%XMM1_4 = %99
%YMM1_4 = %99
%ZMM1_4 = %99
%RIP_16 = 4195747
%EIP_15 = 4195747
%107 = %99
%108 = %99
%110 = %RSP-32
%111 = [%RSP-32]
%112 = fsub double %99, %111
%113 = %112
%XMM1_5 = %112
%YMM1_5 = %112
%ZMM1_5 = %112
%RIP_17 = 4195752
%EIP_16 = 4195752
%120 = %112
%121 = %112
%123 = %RSP-48
store double %112, double* %RSP-48, align 1
%RIP_18 = 4195757
%EIP_17 = 4195757
%125 = %RSP-24
%126 = [%RSP-24]
%127 = %126
%XMM1_6 = %126
%YMM1_6 = %126
%ZMM1_6 = %126
%RIP_19 = 4195762
%EIP_18 = 4195762
%134 = %126
%135 = %126
%137 = %RSP-32
%138 = [%RSP-32]
%139 = fmul double %126, %138
%140 = %139
%XMM1_7 = %139
%YMM1_7 = %139
%ZMM1_7 = %139
%RIP_20 = 4195767
%EIP_19 = 4195767
%147 = %139
%148 = %139
%150 = %RSP-56
store double %139, double* %RSP-56, align 1
%RIP_21 = 4195772
%EIP_20 = 4195772
%152 = %RSP-24
%153 = [%RSP-24]
%154 = %153
%XMM1_8 = %153
%YMM1_8 = %153
%ZMM1_8 = %153
%RIP_22 = 4195777
%EIP_21 = 4195777
%161 = %153
%162 = %153
%164 = %RSP-32
%165 = [%RSP-32]
%166 = fdiv double %153, %165
%167 = %166
%XMM1_9 = %166
%YMM1_9 = %166
%ZMM1_9 = %166
%RIP_23 = 4195782
%EIP_22 = 4195782
%174 = %166
%175 = %166
%177 = %RSP-64
store double %166, double* %RSP-64, align 1
%RIP_24 = 4195787
%EIP_23 = 4195787
%179 = %RSP-40
%180 = [%RSP-40]
%181 = %180
%XMM1_10 = %180
%YMM1_10 = %180
%ZMM1_10 = %180
%RIP_25 = 4195792
%EIP_24 = 4195792
%188 = %180
%189 = %180
%191 = %RSP-48
%192 = [%RSP-48]
%193 = fmul double %180, %192
%194 = %193
%XMM1_11 = %193
%YMM1_11 = %193
%ZMM1_11 = %193
%RIP_26 = 4195797
%EIP_25 = 4195797
%201 = %193
%202 = %193
%204 = %RSP-56
%205 = [%RSP-56]
%206 = fdiv double %193, %205
%207 = %206
%XMM1_12 = %206
%YMM1_12 = %206
%ZMM1_12 = %206
%RIP_27 = 4195802
%EIP_26 = 4195802
%214 = %206
%215 = %206
%217 = %RSP-64
%218 = [%RSP-64]
%219 = fadd double %206, %218
%220 = %219
%XMM1_13 = %219
%YMM1_13 = %219
%ZMM1_13 = %219
%RIP_28 = 4195807
%EIP_27 = 4195807
%227 = %219
%228 = %219
%230 = %RSP-72
store double %219, double* %RSP-72, align 1
%RIP_29 = 4195812
%EIP_28 = 4195812
%231 = %31
%232 = %31
%234 = %RSP-72
%235 = [%RSP-72]
%ZF_0 = fcmp ueq double %232, %235
%PF_0 = fcmp uno double %31, %235
%CF_0 = fcmp ult double %31, %235
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
store i32 %RSP-8, i32* %EBP
store i32 4195831, i32* %EIP
store i32 %RSP-88, i32* %ESP
store i64 1336630430, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 4195831, i64* %RIP
store i64 %RSP-88, i64* %RSP
%253 = %31
store <4 x float> %31, <4 x float>* %XMM0
%254 = %219
store <4 x float> %219, <4 x float>* %XMM1
%255 = 1336630430
store <4 x float> 1336630430, <4 x float>* %XMM2
%256 = %31
store <8 x float> %31, <8 x float>* %YMM0
%257 = %219
store <8 x float> %219, <8 x float>* %YMM1
%258 = 1336630430
store <8 x float> 1336630430, <8 x float>* %YMM2
%259 = %31
store <16 x float> %31, <16 x float>* %ZMM0
%260 = %219
store <16 x float> %219, <16 x float>* %ZMM1
%261 = 1336630430
store <16 x float> 1336630430, <16 x float>* %ZMM2
br i1 %CC_BE_0, label %bb_4005F7, label %bb_4005EA
bb_4005EA: ; preds = %bb_400550
%RIP_33 = 4195823
%EIP_31 = 4195823
%RBP_1 = %RBP
%263 = %RBP-32
%264 = [%RBP-32]
%EAX_2 = %RBP-32
%RAX_2 = %RAX
%RAX_3 = %RBP-32
%265 = lshr i32 %RBP-32, 8
%RIP_34 = 4195826
%EIP_32 = 4195826
%267 = %RBP-4
store i32 %RBP-32, i32* %RBP-4, align 1
%RIP_35 = 4195831
%EIP_33 = 4195831
store i32 %RBP-32, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RBP-32, i64* %RAX
store i64 %RBP, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_4005F7: ; preds = %bb_400550
%RIP_38 = 4195834
%EIP_35 = 4195834
%ZMM0_2 = %ZMM0
%268 = %ZMM0
%XMM0_2 = %ZMM0
%269 = %ZMM0
%270 = %ZMM0
%271 = %ZMM0
%272 = %ZMM0
%273 = xor <2 x i64> %ZMM0, %ZMM0
%XMM0_3 = %273
%274 = %ZMM0
%YMM0_2 = %ZMM0
%YMM0_3 = %273
%277 = %ZMM0
%ZMM0_3 = %273
%RIP_39 = 4195839
%EIP_36 = 4195839
%RBP_2 = %RBP
%281 = %RBP-64
%282 = [%RBP-64]
%283 = %282
%ZMM1_14 = %ZMM1
%284 = %ZMM1
%XMM1_14 = %ZMM1
%XMM1_15 = %282
%287 = %ZMM1
%YMM1_14 = %ZMM1
%YMM1_15 = %282
%290 = %ZMM1
%ZMM1_15 = %282
%RIP_40 = 4195843
%EIP_37 = 4195843
%293 = %282
%294 = %282
%295 = %273
%296 = %273
%ZF_01 = fcmp ueq double %294, %296
%PF_02 = fcmp uno double %282, %273
%CF_03 = fcmp ult double %282, %273
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
store i64 %RBP, i64* %RBP
store i64 4195888, i64* %RIP
%314 = %273
store <4 x float> %273, <4 x float>* %XMM0
%315 = %282
store <4 x float> %282, <4 x float>* %XMM1
%316 = %273
store <8 x float> %273, <8 x float>* %YMM0
%317 = %282
store <8 x float> %282, <8 x float>* %YMM1
%318 = %273
store <16 x float> %273, <16 x float>* %ZMM0
%319 = %282
store <16 x float> %282, <16 x float>* %ZMM1
br i1 %CF_03, label %bb_400630, label %bb_400609
bb_400609: ; preds = %bb_4005F7
%RIP_50 = 4195859
%EIP_45 = 4195859
%RIP_51 = 4195864
%EIP_46 = 4195864
%320 = 5
%321 = 5
%ZMM0_4 = %ZMM0
%322 = %ZMM0
%XMM0_4 = %ZMM0
%XMM0_5 = 5
%325 = %ZMM0
%YMM0_4 = %ZMM0
%YMM0_5 = 5
%328 = %ZMM0
%ZMM0_5 = 5
%RIP_52 = 4195869
%EIP_47 = 4195869
%331 = 5
%332 = 5
%RBP_5 = %RBP
%334 = %RBP-64
%335 = [%RBP-64]
%ZF_07 = fcmp ueq double %332, %335
%PF_08 = fcmp uno double 5, %335
%CF_09 = fcmp ult double 5, %335
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
store i64 %RBP, i64* %RBP
store i64 4195888, i64* %RIP
%353 = 5
store <4 x float> 5, <4 x float>* %XMM0
%354 = 5
store <8 x float> 5, <8 x float>* %YMM0
%355 = 5
store <16 x float> 5, <16 x float>* %ZMM0
br i1 %CC_BE_010, label %bb_400630, label %bb_400623
bb_400623: ; preds = %bb_400609
%RIP_63 = 4195880
%EIP_56 = 4195880
%RBP_7 = %RBP
%357 = %RBP-40
%358 = [%RBP-40]
%EAX_6 = %RBP-40
%RAX_8 = %RAX
%RAX_9 = %RBP-40
%359 = lshr i32 %RBP-40, 8
%RIP_64 = 4195883
%EIP_57 = 4195883
%361 = %RBP-4
store i32 %RBP-40, i32* %RBP-4, align 1
%RIP_65 = 4195888
%EIP_58 = 4195888
store i32 %RBP-40, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RBP-40, i64* %RAX
store i64 %RBP, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_400630: ; preds = %bb_400609, %bb_4005F7
%RIP_56 = 4195898
%EIP_50 = 4195898
%RIP_57 = 4195903
%EIP_51 = 4195903
%362 = 5
%363 = 5
%ZMM0_6 = %ZMM0
%364 = %ZMM0
%XMM0_6 = %ZMM0
%XMM0_7 = 5
%367 = %ZMM0
%YMM0_6 = %ZMM0
%YMM0_7 = 5
%370 = %ZMM0
%ZMM0_7 = 5
%RIP_58 = 4195908
%EIP_52 = 4195908
%RBP_6 = %RBP
%374 = %RBP-64
%375 = [%RBP-64]
%376 = %375
%ZMM1_16 = %ZMM1
%377 = %ZMM1
%XMM1_16 = %ZMM1
%XMM1_17 = %375
%380 = %ZMM1
%YMM1_16 = %ZMM1
%YMM1_17 = %375
%383 = %ZMM1
%ZMM1_17 = %375
%RIP_59 = 4195912
%EIP_53 = 4195912
%386 = %375
%387 = %375
%388 = 5
%389 = 5
%ZF_011 = fcmp ueq double %387, %389
%PF_012 = fcmp uno double %375, 5
%CF_013 = fcmp ult double %375, 5
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
store i64 %RBP, i64* %RBP
store i64 4195937, i64* %RIP
%407 = 5
store <4 x float> 5, <4 x float>* %XMM0
%408 = %375
store <4 x float> %375, <4 x float>* %XMM1
%409 = 5
store <8 x float> 5, <8 x float>* %YMM0
%410 = %375
store <8 x float> %375, <8 x float>* %YMM1
%411 = 5
store <16 x float> 5, <16 x float>* %ZMM0
%412 = %375
store <16 x float> %375, <16 x float>* %ZMM1
br i1 %CC_NE_0, label %bb_400661, label %bb_40064E
bb_40064E: ; preds = %bb_400630
%RIP_68 = 4195924
%EIP_60 = 4195924
store i32 4195937, i32* %EIP
store i64 4195937, i64* %RIP
bb_400654: ; preds = %bb_40064E
%RIP_74 = 4195929
%EIP_64 = 4195929
%RBP_8 = %RBP
%415 = %RBP-48
%416 = [%RBP-48]
%EAX_7 = %RBP-48
%RAX_10 = %RAX
%RAX_11 = %RBP-48
%417 = lshr i32 %RBP-48, 8
%RIP_75 = 4195932
%EIP_65 = 4195932
%419 = %RBP-4
store i32 %RBP-48, i32* %RBP-4, align 1
%RIP_76 = 4195937
%EIP_66 = 4195937
store i32 %RBP-48, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RBP-48, i64* %RAX
store i64 %RBP, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_400661: ; preds = %bb_40064E, %bb_400630
%RIP_71 = 4195942
%EIP_62 = 4195942
store i32 4195942, i32* %EIP
store i64 4195942, i64* %RIP
br label %bb_400666
bb_400666: ; preds = %bb_400678, %bb_400661
%RIP_79 = 4195945
%EIP_68 = 4195945
%ZMM0_8 = %ZMM0
%420 = %ZMM0
%XMM0_8 = %ZMM0
%421 = %ZMM0
%422 = %ZMM0
%423 = %ZMM0
%424 = %ZMM0
%425 = xor <2 x i64> %ZMM0, %ZMM0
%XMM0_9 = %425
%426 = %ZMM0
%YMM0_8 = %ZMM0
%YMM0_9 = %425
%429 = %ZMM0
%ZMM0_9 = %425
%RIP_80 = 4195950
%EIP_69 = 4195950
%RBP_9 = %RBP
%433 = %RBP-64
%434 = [%RBP-64]
%435 = %434
%ZMM1_18 = %ZMM1
%436 = %ZMM1
%XMM1_18 = %ZMM1
%XMM1_19 = %434
%439 = %ZMM1
%YMM1_18 = %ZMM1
%YMM1_19 = %434
%442 = %ZMM1
%ZMM1_19 = %434
%RIP_81 = 4195954
%EIP_70 = 4195954
%445 = %434
%446 = %434
%447 = %425
%448 = %425
%ZF_015 = fcmp ueq double %446, %448
%PF_016 = fcmp uno double %434, %425
%CF_017 = fcmp ult double %434, %425
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
store i64 %RBP, i64* %RBP
store i64 4196011, i64* %RIP
%466 = %425
store <4 x float> %425, <4 x float>* %XMM0
%467 = %434
store <4 x float> %434, <4 x float>* %XMM1
%468 = %425
store <8 x float> %425, <8 x float>* %YMM0
%469 = %434
store <8 x float> %434, <8 x float>* %YMM1
%470 = %425
store <16 x float> %425, <16 x float>* %ZMM0
%471 = %434
store <16 x float> %434, <16 x float>* %ZMM1
br i1 %CC_BE_018, label %bb_4006AB, label %bb_400678
bb_400678: ; preds = %bb_400666
%RIP_85 = 4195968
%EIP_73 = 4195968
%RIP_86 = 4195973
%EIP_74 = 4195973
%RBP_10 = %RBP
%473 = %RBP-64
%474 = [%RBP-64]
%475 = %474
%ZMM0_10 = %ZMM0
%476 = %ZMM0
%XMM0_10 = %ZMM0
%XMM0_11 = %474
%479 = %ZMM0
%YMM0_10 = %ZMM0
%YMM0_11 = %474
%482 = %ZMM0
%ZMM0_11 = %474
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
store i64 4195980, i64* %RSP-8
%ESP_5 = %RSP-8
store i32 %EAX_9, i32* %EAX
store i32 4196192, i32* %EDI
store i32 4195980, i32* %EIP
store i32 %RSP-8, i32* %ESP
store i64 %RAX_13, i64* %RAX
store i64 %RBP, i64* %RBP
store i64 4196192, i64* %RDI
store i64 4195980, i64* %RIP
store i64 %RSP_8, i64* %RSP
%489 = %474
store <4 x float> %474, <4 x float>* %XMM0
%490 = %474
store <8 x float> %474, <8 x float>* %YMM0
%491 = %474
store <16 x float> %474, <16 x float>* %ZMM0
%494 = [%RAX]
store i64 %494, i64* %RAX_ptr
%495 = [%RBP]
store i64 %495, i64* %RBP_ptr
%496 = [%RDI]
store i64 %496, i64* %RDI_ptr
%497 = [%RIP]
store i64 %497, i64* %RIP_ptr
%498 = [%RSP]
store i64 %498, i64* %RSP_ptr
%499 = [%ZMM0]
store <16 x float> %499, <16 x float>* %ZMM0_ptr
%500 = [%ZMM1]
store <16 x float> %500, <16 x float>* %ZMM1_ptr
%501 = [%ZMM2]
store <16 x float> %501, <16 x float>* %ZMM2_ptr
call void @fn_400410(%regset* %0)
%504 = [%RAX_ptr]
store i64 %504, i64* %RAX
%505 = [%RBP_ptr]
store i64 %505, i64* %RBP
%506 = [%RDI_ptr]
store i64 %506, i64* %RDI
%507 = [%RIP_ptr]
store i64 %507, i64* %RIP
%508 = [%RSP_ptr]
store i64 %508, i64* %RSP
%509 = [%ZMM0_ptr]
store <16 x float> %509, <16 x float>* %ZMM0
%510 = [%ZMM1_ptr]
store <16 x float> %510, <16 x float>* %ZMM1
%511 = [%ZMM2_ptr]
store <16 x float> %511, <16 x float>* %ZMM2
%RIP_89 = %RIP
%RIP_90 = %RIP+9
%EIP_77 = %RIP+9
%512 = 4196184
%513 = [4196184]
%514 = [4196184]
%ZMM0_12 = %ZMM0
%515 = %ZMM0
%XMM0_12 = %ZMM0
%XMM0_13 = [4196184]
%518 = %ZMM0
%YMM0_12 = %ZMM0
%YMM0_13 = [4196184]
%521 = %ZMM0
%ZMM0_13 = [4196184]
%RIP_91 = %RIP+14
%EIP_78 = %RIP+14
%RBP_11 = %RBP
%525 = %RBP-64
%526 = [%RBP-64]
%527 = %526
%ZMM1_20 = %ZMM1
%528 = %ZMM1
%XMM1_20 = %ZMM1
%XMM1_21 = %526
%531 = %ZMM1
%YMM1_20 = %ZMM1
%YMM1_21 = %526
%534 = %ZMM1
%ZMM1_21 = %526
%RIP_92 = %RIP+18
%EIP_79 = %RIP+18
%537 = %526
%538 = %526
%539 = [4196184]
%540 = [4196184]
%541 = fsub double %526, [4196184]
%542 = %541
%XMM1_22 = %541
%YMM1_22 = %541
%ZMM1_22 = %541
%RIP_93 = %RIP+23
%EIP_80 = %RIP+23
%549 = %541
%550 = %541
%552 = %RBP-64
store double %541, double* %RBP-64, align 1
%RIP_94 = %RIP+26
%EIP_81 = %RIP+26
%RAX_14 = %RAX
%EAX_10 = %RAX
%554 = %RBP-68
store i32 %RAX, i32* %RBP-68, align 1
%RIP_95 = %RIP+31
%EIP_82 = %RIP+31
store i32 %RAX, i32* %EAX
store i32 4195942, i32* %EIP
store i64 %RAX, i64* %RAX
store i64 %RBP, i64* %RBP
store i64 4195942, i64* %RIP
%555 = [4196184]
store <4 x float> [4196184], <4 x float>* %XMM0
%556 = %541
store <4 x float> %541, <4 x float>* %XMM1
%557 = [4196184]
store <8 x float> [4196184], <8 x float>* %YMM0
%558 = %541
store <8 x float> %541, <8 x float>* %YMM1
%559 = [4196184]
store <16 x float> [4196184], <16 x float>* %ZMM0
%560 = %541
store <16 x float> %541, <16 x float>* %ZMM1
br label %bb_400666
bb_4006AB: ; preds = %bb_400666
%RIP_98 = 4196016
%EIP_84 = 4196016
%RBP_12 = %RBP
%562 = %RBP-64
%563 = [%RBP-64]
%564 = %563
%ZMM0_14 = %ZMM0
%565 = %ZMM0
%XMM0_14 = %ZMM0
%XMM0_15 = %563
%568 = %ZMM0
%YMM0_14 = %ZMM0
%YMM0_15 = %563
%571 = %ZMM0
%ZMM0_15 = %563
%RIP_99 = 4196021
%EIP_85 = 4196021
%RSP_9 = %RSP
%574 = %RSP-8
store i64 4196021, i64* %RSP-8
%ESP_6 = %RSP-8
store i32 4196021, i32* %EIP
store i32 %RSP-8, i32* %ESP
store i64 %RBP, i64* %RBP
store i64 4196021, i64* %RIP
store i64 %RSP_10, i64* %RSP
%575 = %563
store <4 x float> %563, <4 x float>* %XMM0
%576 = %563
store <8 x float> %563, <8 x float>* %YMM0
%577 = %563
store <16 x float> %563, <16 x float>* %ZMM0
%580 = [%RAX]
store i64 %580, i64* %RAX_ptr
%581 = [%RBP]
store i64 %581, i64* %RBP_ptr
%582 = [%RDI]
store i64 %582, i64* %RDI_ptr
%583 = [%RIP]
store i64 %583, i64* %RIP_ptr
%584 = [%RSP]
store i64 %584, i64* %RSP_ptr
%585 = [%ZMM0]
store <16 x float> %585, <16 x float>* %ZMM0_ptr
%586 = [%ZMM1]
store <16 x float> %586, <16 x float>* %ZMM1_ptr
%587 = [%ZMM2]
store <16 x float> %587, <16 x float>* %ZMM2_ptr
call void @fn_400530(%regset* %0)
%590 = [%RAX_ptr]
store i64 %590, i64* %RAX
%591 = [%RBP_ptr]
store i64 %591, i64* %RBP
%592 = [%RDI_ptr]
store i64 %592, i64* %RDI
%593 = [%RIP_ptr]
store i64 %593, i64* %RIP
%594 = [%RSP_ptr]
store i64 %594, i64* %RSP
%595 = [%ZMM0_ptr]
store <16 x float> %595, <16 x float>* %ZMM0
%596 = [%ZMM1_ptr]
store <16 x float> %596, <16 x float>* %ZMM1
%597 = [%ZMM2_ptr]
store <16 x float> %597, <16 x float>* %ZMM2
%RIP_100 = %RIP
%RIP_101 = %RIP+4
%EIP_86 = %RIP+4
%ZMM0_16 = %ZMM0
%598 = %ZMM0
%XMM0_16 = %ZMM0
%599 = %ZMM0
%600 = %ZMM0
%EAX_11 = %ZMM0
%RAX_15 = %RAX
%RAX_16 = %ZMM0
%601 = lshr i32 %ZMM0, 8
%RIP_102 = %RIP+7
%EIP_87 = %RIP+7
%RBP_13 = %RBP
%603 = %RBP-4
store i32 %ZMM0, i32* %RBP-4, align 1
store i32 %ZMM0, i32* %EAX
store i32 %RIP+7, i32* %EIP
store i64 %ZMM0, i64* %RAX
store i64 %RBP, i64* %RBP
store i64 %RIP+7, i64* %RIP
%604 = %ZMM0
store <4 x float> %ZMM0, <4 x float>* %XMM0
store <16 x float> %ZMM0, <16 x float>* %ZMM0
br label %bb_4006BC
bb_4006BC: ; preds = %bb_4006AB, %bb_400654, %bb_400623, %bb_4005EA
%RIP_44 = 4196031
%EIP_40 = 4196031
%RBP_3 = %RBP
%606 = %RBP-4
%EAX_3 = [%RBP-4]
%RAX_4 = %RAX
%RAX_5 = %RBP-4
%607 = lshr i32 [%RBP-4], 8
%RIP_45 = 4196035
%EIP_41 = 4196035
%RSP_3 = %RSP
%RSP_4 = %RSP+80
%ESP_2 = %RSP+80
%RIP_46 = 4196036
%EIP_42 = 4196036
%RSP_5 = %RSP+88
%ESP_3 = %RSP+88
%609 = %RSP+80
%RBP_4 = [%RSP+96]
%EBP_1 = %RSP+96
%RIP_47 = 4196037
%EIP_43 = 4196037
%610 = %RSP+96
%RIP_48 = [%RSP+88]
%ESP_4 = %RSP+96
%EIP_44 = %RSP+88
%ZF_04 = icmp eq i64 %RSP_4, 0
%SF_0 = icmp slt i64 %RSP+80, 0
%611 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP, i64 80)
%OF_0 = extractvalue { i64, i1 } %611, 1
%612 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP, i64 80)
%CF_05 = extractvalue { i64, i1 } %612, 1
%613 = %RSP+80
%614 = call i8 @llvm.ctpop.i8(i8 %RSP+80)
%615 = %614
%PF_06 = icmp eq i1 %614, false
%616 = %612
%617 = shl i32 %612, 0
%619 = %PF_06
%620 = shl i32 %PF_06, 2
%622 = false
%623 = shl i32 false, 4
%625 = %ZF_04
%626 = shl i32 %ZF_04, 6
%628 = %SF_0
%629 = shl i32 %SF_0, 7
%631 = %611
%632 = shl i32 %611, 11
store i32 [%RBP-4], i32* %EAX
store i32 %RSP+96, i32* %EBP
store i32 %RSP+88, i32* %EIP
store i32 %RSP+96, i32* %ESP
store i64 %RBP-4, i64* %RAX
store i64 [%RSP+96], i64* %RBP
store i64 [%RSP+88], i64* %RIP
store i64 %RSP_6, i64* %RSP
br label %exit_fn_400550
}

