define void @fn_400550(%regset* noalias nocapture) {
entry_fn_400550:
%RIP_init = %regset*
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %regset*
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
%RBP_init = %regset*
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_init = %regset*
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP_init = %regset*
%ESP = alloca i32
store i32 %ESP_init, i32* %ESP
%EBP_init = %regset*
%EBP = alloca i32
store i32 %EBP_init, i32* %EBP
%ZMM0_init = %regset*
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%XMM0_init = %regset*
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%YMM0_init = %regset*
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%RAX_init = %regset*
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %regset*
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%ZMM1_init = %regset*
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%XMM1_init = %regset*
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%YMM1_init = %regset*
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%ZMM2_init = %regset*
%ZMM2 = alloca <16 x float>
store <16 x float> %ZMM2_init, <16 x float>* %ZMM2
%XMM2_init = %regset*
%XMM2 = alloca <4 x float>
store <4 x float> %XMM2_init, <4 x float>* %XMM2
%YMM2_init = %regset*
%YMM2 = alloca <8 x float>
store <8 x float> %YMM2_init, <8 x float>* %YMM2
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
%RBP_0 = %RBP
%25 = %RSP-8
store i64 %RBP_0, i64* %25, align 1
%RSP_1 = %RSP-8
%EBP_0 = %RSP-8
%RSP_2 = %RSP-88
%ESP_1 = %RSP-88
%XMM0_1 = %31
%39 = 4003244811
%XMM1_0 = %ZMM1
%XMM1_1 = %XMM1_0 : %39
%50 = 1336630430
%XMM2_0 = %ZMM2
%XMM2_1 = %XMM2_0 : %50
%61 = %RSP-12
store i32 0, i32* %61, align 1
%65 = %RSP-32
store double %50, double* %65, align 1
%69 = %RSP-24
store double %39, double* %69, align 1
%71 = %RSP-24
%72 = load double, double* %71, align 1
%73 = %72
%XMM1_2 = %XMM1_1 : %73
%83 = %RSP-32
%84 = load double, double* %83, align 1
%85 = fadd double %73, %84
%86 = %85
%XMM1_3 = %XMM1_2 : %86
%96 = %RSP-40
store double %86, double* %96, align 1
%98 = %RSP-24
%99 = load double, double* %98, align 1
%100 = %99
%XMM1_4 = %XMM1_3 : %100
%110 = %RSP-32
%111 = load double, double* %110, align 1
%112 = fsub double %100, %111
%113 = %112
%XMM1_5 = %XMM1_4 : %113
%123 = %RSP-48
store double %113, double* %123, align 1
%125 = %RSP-24
%126 = load double, double* %125, align 1
%127 = %126
%XMM1_6 = %XMM1_5 : %127
%137 = %RSP-32
%138 = load double, double* %137, align 1
%139 = fmul double %127, %138
%140 = %139
%XMM1_7 = %XMM1_6 : %140
%150 = %RSP-56
store double %140, double* %150, align 1
%152 = %RSP-24
%153 = load double, double* %152, align 1
%154 = %153
%XMM1_8 = %XMM1_7 : %154
%164 = %RSP-32
%165 = load double, double* %164, align 1
%166 = fdiv double %154, %165
%167 = %166
%XMM1_9 = %XMM1_8 : %167
%177 = %RSP-64
store double %167, double* %177, align 1
%179 = %RSP-40
%180 = load double, double* %179, align 1
%181 = %180
%XMM1_10 = %XMM1_9 : %181
%191 = %RSP-48
%192 = load double, double* %191, align 1
%193 = fmul double %181, %192
%194 = %193
%XMM1_11 = %XMM1_10 : %194
%204 = %RSP-56
%205 = load double, double* %204, align 1
%206 = fdiv double %194, %205
%207 = %206
%XMM1_12 = %XMM1_11 : %207
%217 = %RSP-64
%218 = load double, double* %217, align 1
%219 = fadd double %207, %218
%220 = %219
%XMM1_13 = %XMM1_12 : %220
%230 = %RSP-72
store double %220, double* %230, align 1
%231 = %31
%232 = %231
%234 = %RSP-72
%235 = load double, double* %234, align 1
%ZF_0 = fcmp ueq double %232, %235
%CF_0 = fcmp ult double %231, %235
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
store <4 x float> %220, <4 x float>* %XMM1
store <4 x float> %50, <4 x float>* %XMM2
store <8 x float> %XMM0_1, <8 x float>* %YMM0
store <8 x float> %XMM1_13, <8 x float>* %YMM1
store <8 x float> %XMM2_1, <8 x float>* %YMM2
store <16 x float> %XMM0_1, <16 x float>* %ZMM0
store <16 x float> %XMM1_13, <16 x float>* %ZMM1
store <16 x float> %XMM2_1, <16 x float>* %ZMM2
br i1 %CC_BE_0, label %bb_4005F7, label %bb_4005EA
bb_4005EA:                                        ; preds = %bb_400550
%RBP_1 = %RBP
%EAX_2 = %RBP-32
%RAX_3 = %RBP-32
%267 = %RBP-4
store i32 %EAX_2, i32* %267, align 1
store i32 %EAX_2, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_3, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_4005F7:                                        ; preds = %bb_400550
%XMM0_3 = %273
%RBP_2 = %RBP
%281 = %RBP-64
%282 = load double, double* %281, align 1
%283 = %282
%XMM1_14 = %ZMM1
%XMM1_15 = %XMM1_14 : %283
%295 = %273
%CF_03 = fcmp ult double %283, %295
store i32 4195888, i32* %EIP
store i64 %RBP_2, i64* %RBP
store i64 4195888, i64* %RIP
%314 = %273
store <4 x float> %314, <4 x float>* %XMM0
store <4 x float> %283, <4 x float>* %XMM1
store <8 x float> %XMM0_3, <8 x float>* %YMM0
store <8 x float> %XMM1_15, <8 x float>* %YMM1
store <16 x float> %XMM0_3, <16 x float>* %ZMM0
store <16 x float> %XMM1_15, <16 x float>* %ZMM1
br i1 %CF_03, label %bb_400630, label %bb_400609
bb_400609:                                        ; preds = %bb_4005F7
%321 = 5
%XMM0_4 = %ZMM0
%XMM0_5 = %XMM0_4 : %321
%332 = %321
%RBP_5 = %RBP
%334 = %RBP-64
%335 = load double, double* %334, align 1
%ZF_07 = fcmp ueq double %332, %335
%CF_09 = fcmp ult double %321, %335
%CC_BE_010 = or i1 %CF_09, %ZF_07
store i32 5, i32* %EAX
store i32 4195888, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 4195888, i64* %RIP
store <4 x float> %321, <4 x float>* %XMM0
store <8 x float> %XMM0_5, <8 x float>* %YMM0
store <16 x float> %XMM0_5, <16 x float>* %ZMM0
br i1 %CC_BE_010, label %bb_400630, label %bb_400623
bb_400623:                                        ; preds = %bb_400609
%RBP_7 = %RBP
%EAX_6 = %RBP-40
%RAX_9 = %RBP-40
%361 = %RBP-4
store i32 %EAX_6, i32* %361, align 1
store i32 %EAX_6, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_9, i64* %RAX
store i64 %RBP_7, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_400630:                                        ; preds = %bb_400609, %bb_4005F7
%363 = 5
%XMM0_6 = %ZMM0
%XMM0_7 = %XMM0_6 : %363
%RBP_6 = %RBP
%374 = %RBP-64
%375 = load double, double* %374, align 1
%376 = %375
%XMM1_16 = %ZMM1
%XMM1_17 = %XMM1_16 : %376
%387 = %376
%389 = %363
%ZF_011 = fcmp ueq double %387, %389
%CC_NE_0 = xor i1 %ZF_011, true
store i32 5, i32* %EAX
store i32 4195937, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195937, i64* %RIP
store <4 x float> %363, <4 x float>* %XMM0
store <4 x float> %376, <4 x float>* %XMM1
store <8 x float> %XMM0_7, <8 x float>* %YMM0
store <8 x float> %XMM1_17, <8 x float>* %YMM1
store <16 x float> %XMM0_7, <16 x float>* %ZMM0
store <16 x float> %XMM1_17, <16 x float>* %ZMM1
br i1 %CC_NE_0, label %bb_400661, label %bb_40064E
bb_40064E:                                        ; preds = %bb_400630
store i32 4195937, i32* %EIP
store i64 4195937, i64* %RIP
bb_400654:                                        ; preds = %bb_40064E
%RBP_8 = %RBP
%EAX_7 = %RBP-48
%RAX_11 = %RBP-48
%419 = %RBP-4
store i32 %EAX_7, i32* %419, align 1
store i32 %EAX_7, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_11, i64* %RAX
store i64 %RBP_8, i64* %RBP
store i64 4196028, i64* %RIP
br label %bb_4006BC
bb_400661:                                        ; preds = %bb_40064E, %bb_400630
store i32 4195942, i32* %EIP
store i64 4195942, i64* %RIP
br label %bb_400666
bb_400666:                                        ; preds = %bb_400678, %bb_400661
%XMM0_9 = %425
%RBP_9 = %RBP
%433 = %RBP-64
%434 = load double, double* %433, align 1
%435 = %434
%XMM1_18 = %ZMM1
%XMM1_19 = %XMM1_18 : %435
%446 = %435
%447 = %425
%448 = %447
%ZF_015 = fcmp ueq double %446, %448
%CF_017 = fcmp ult double %435, %447
%CC_BE_018 = or i1 %CF_017, %ZF_015
store i32 4196011, i32* %EIP
store i64 %RBP_9, i64* %RBP
store i64 4196011, i64* %RIP
%466 = %425
store <4 x float> %466, <4 x float>* %XMM0
store <4 x float> %435, <4 x float>* %XMM1
store <8 x float> %XMM0_9, <8 x float>* %YMM0
store <8 x float> %XMM1_19, <8 x float>* %YMM1
store <16 x float> %XMM0_9, <16 x float>* %ZMM0
store <16 x float> %XMM1_19, <16 x float>* %ZMM1
br i1 %CC_BE_018, label %bb_4006AB, label %bb_400678
bb_400678:                                        ; preds = %bb_400666
%RBP_10 = %RBP
%473 = %RBP-64
%474 = load double, double* %473, align 1
%475 = %474
%XMM0_10 = %ZMM0
%XMM0_11 = %XMM0_10 : %475
%486 = and i32 %RAX, -256
%EAX_9 = or i32 %486, 1
%487 = and i64 %RAX, -256
%RAX_13 = or i64 %487, 1
%RIP_88 = 4195980
%EIP_76 = 4195980
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
store <4 x float> %475, <4 x float>* %XMM0
store <8 x float> %XMM0_11, <8 x float>* %YMM0
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
%513 = [ 4196184 ]
%514 = %513
%XMM0_12 = %ZMM0
%XMM0_13 = %XMM0_12 : %514
%RBP_11 = %RBP
%525 = %RBP-64
%526 = load double, double* %525, align 1
%527 = %526
%XMM1_20 = %ZMM1
%XMM1_21 = %XMM1_20 : %527
%541 = fsub double %527, %514
%542 = %541
%XMM1_22 = %XMM1_21 : %542
%552 = %RBP-64
store double %542, double* %552, align 1
%RAX_14 = %RAX
%EAX_10 = %RAX
%554 = %RBP-68
store i32 %EAX_10, i32* %554, align 1
store i32 %EAX_10, i32* %EAX
store i32 4195942, i32* %EIP
store i64 %RAX_14, i64* %RAX
store i64 %RBP_11, i64* %RBP
store i64 4195942, i64* %RIP
store <4 x float> %514, <4 x float>* %XMM0
store <4 x float> %542, <4 x float>* %XMM1
store <8 x float> %XMM0_13, <8 x float>* %YMM0
store <8 x float> %XMM1_22, <8 x float>* %YMM1
store <16 x float> %XMM0_13, <16 x float>* %ZMM0
store <16 x float> %XMM1_22, <16 x float>* %ZMM1
br label %bb_400666
bb_4006AB:                                        ; preds = %bb_400666
%RBP_12 = %RBP
%562 = %RBP-64
%563 = load double, double* %562, align 1
%564 = %563
%XMM0_14 = %ZMM0
%XMM0_15 = %XMM0_14 : %564
%RIP_99 = 4196021
%EIP_85 = 4196021
%574 = %RSP-8
store i64 4196021, i64* %574
%ESP_6 = %RSP-8
store i32 %EIP_85, i32* %EIP
store i32 %ESP_6, i32* %ESP
store i64 %RBP_12, i64* %RBP
store i64 %RIP_99, i64* %RIP
store i64 %RSP_10, i64* %RSP
store <4 x float> %564, <4 x float>* %XMM0
store <8 x float> %XMM0_15, <8 x float>* %YMM0
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
%ZMM0_16 = %ZMM0
%EAX_11 = %ZMM0
%RAX_16 = %ZMM0
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
%EAX_3 = [ %RBP-4 ]
%RAX_5 = %RBP-4
%RBP_4 = [ %RSP+96 ]
%EBP_1 = %RSP+96
%RIP_48 = [ %RSP+88 ]
%ESP_4 = %RSP+96
%EIP_44 = %RSP+88
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

