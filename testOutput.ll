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
br label  %bb_0
exit_fn_400550:                                   ; preds =  %bb_12
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
; <label>:bb_0
%RBP_0 = %RBP
%25 = %RSP_0 -8
store i64 %RBP_0, i64* %25, align 1
%RSP_1 = %RSP -8
%EBP_0 = %RSP -8
%RSP_2 = %RSP -88
%ESP_1 = %RSP -88
%XMM0_1 = %31
%61 = %RSP_1 -4
store i32 0, i32* %61, align 1
%65 = %RSP_1 -24
store i128 %XMM2_1, double* %65, align 1
%69 = %RSP_1 -16
store i128 %XMM1_1, double* %69, align 1
%96 = %RSP_1 -32
store i128 %XMM1_3, double* %96, align 1
%123 = %RSP_1 -40
store i128 %XMM1_5, double* %123, align 1
%150 = %RSP_1 -48
store i128 %XMM1_7, double* %150, align 1
%177 = %RSP_1 -56
store i128 %XMM1_9, double* %177, align 1
%230 = %RSP_1 -64
store i128 %XMM1_13, double* %230, align 1
%232 = bitcast i128 %XMM0_1 to double
%234 = %RSP_1 -64
%235 = load double, double* %234, align 1
%ZF_0 = fcmp ueq double %232, %235
%CF_0 = fcmp ult i128 %XMM0_1, %235
%CC_BE_0 = or i1 %CF_0, %ZF_0
store i32 1336630430, i32* %EAX
store i32 %EBP_0, i32* %EBP
store i32 4195831, i32* %EIP
store i32 %ESP_1, i32* %ESP
store i64 1336630430, i64* %RAX
store i64 %RSP_1, i64* %RBP
store i64 4195831, i64* %RIP
store i64 %RSP_2, i64* %RSP
store i128 %XMM0_1, <4 x float>* %XMM0
store i128 %XMM1_13, <4 x float>* %XMM1
store i128 %XMM2_1, <4 x float>* %XMM2
store i256 %YMM0_1, <8 x float>* %YMM0
store i256 %YMM1_13, <8 x float>* %YMM1
store i256 %YMM2_1, <8 x float>* %YMM2
store i512 %ZMM0_1, <16 x float>* %ZMM0
store i512 %ZMM1_13, <16 x float>* %ZMM1
store i512 %ZMM2_1, <16 x float>* %ZMM2
br i1 %CC_BE_0, label  %bb_4005F7, label  %bb_1
; <label>:bb_1
%RBP_1 = %RBP
%EAX_2 = %RBP -32
%RAX_3 = %RBP -32
%267 = %RBP_1 -4
store i32 %EAX_2, i32* %267, align 1
store i32 %EAX_2, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_3, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 4196028, i64* %RIP
br label  %bb_12
; <label>:bb_2
%XMM0_3 = %273
%RBP_2 = %RBP
%CF_03 = fcmp ult i128 %XMM1_15, %XMM0_3
store i32 4195888, i32* %EIP
store i64 %RBP_2, i64* %RBP
store i64 4195888, i64* %RIP
store i128 %XMM0_3, <4 x float>* %XMM0
store i128 %XMM1_15, <4 x float>* %XMM1
store i256 %YMM0_3, <8 x float>* %YMM0
store i256 %YMM1_15, <8 x float>* %YMM1
store i512 %ZMM0_3, <16 x float>* %ZMM0
store i512 %ZMM1_15, <16 x float>* %ZMM1
br i1 %CF_03, label  %bb_400630, label  %bb_3
; <label>:bb_3
%332 = bitcast i128 %XMM0_5 to double
%RBP_5 = %RBP
%334 = %RBP_5 -64
%335 = load double, double* %334, align 1
%ZF_07 = fcmp ueq double %332, %335
%CF_09 = fcmp ult i128 %XMM0_5, %335
%CC_BE_010 = or i1 %CF_09, %ZF_07
store i32 5, i32* %EAX
store i32 4195888, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 4195888, i64* %RIP
store i128 %XMM0_5, <4 x float>* %XMM0
store i256 %YMM0_5, <8 x float>* %YMM0
store i512 %ZMM0_5, <16 x float>* %ZMM0
br i1 %CC_BE_010, label  %bb_400630, label  %bb_4
; <label>:bb_4
%RBP_7 = %RBP
%EAX_6 = %RBP -40
%RAX_9 = %RBP -40
%361 = %RBP_7 -4
store i32 %EAX_6, i32* %361, align 1
store i32 %EAX_6, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_9, i64* %RAX
store i64 %RBP_7, i64* %RBP
store i64 4196028, i64* %RIP
br label  %bb_12
; <label>:bb_5
%RBP_6 = %RBP
%387 = bitcast i128 %XMM1_17 to double
%389 = bitcast i128 %XMM0_7 to double
%ZF_011 = fcmp ueq double %387, %389
%CC_NE_0 = xor i1 %ZF_011, true
store i32 5, i32* %EAX
store i32 4195937, i32* %EIP
store i64 5, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195937, i64* %RIP
store i128 %XMM0_7, <4 x float>* %XMM0
store i128 %XMM1_17, <4 x float>* %XMM1
store i256 %YMM0_7, <8 x float>* %YMM0
store i256 %YMM1_17, <8 x float>* %YMM1
store i512 %ZMM0_7, <16 x float>* %ZMM0
store i512 %ZMM1_17, <16 x float>* %ZMM1
br i1 %CC_NE_0, label  %bb_400661, label  %bb_6
; <label>:bb_6
store i32 4195937, i32* %EIP
store i64 4195937, i64* %RIP
; <label>:bb_7
%RBP_8 = %RBP
%EAX_7 = %RBP -48
%RAX_11 = %RBP -48
%419 = %RBP_8 -4
store i32 %EAX_7, i32* %419, align 1
store i32 %EAX_7, i32* %EAX
store i32 4196028, i32* %EIP
store i64 %RAX_11, i64* %RAX
store i64 %RBP_8, i64* %RBP
store i64 4196028, i64* %RIP
br label  %bb_12
; <label>:bb_8
store i32 4195942, i32* %EIP
store i64 4195942, i64* %RIP
br label  %bb_9
; <label>:bb_9
%XMM0_9 = %425
%RBP_9 = %RBP
%446 = bitcast i128 %XMM1_19 to double
%448 = bitcast i128 %XMM0_9 to double
%ZF_015 = fcmp ueq double %446, %448
%CF_017 = fcmp ult i128 %XMM1_19, %XMM0_9
%CC_BE_018 = or i1 %CF_017, %ZF_015
store i32 4196011, i32* %EIP
store i64 %RBP_9, i64* %RBP
store i64 4196011, i64* %RIP
store i128 %XMM0_9, <4 x float>* %XMM0
store i128 %XMM1_19, <4 x float>* %XMM1
store i256 %YMM0_9, <8 x float>* %YMM0
store i256 %YMM1_19, <8 x float>* %YMM1
store i512 %ZMM0_9, <16 x float>* %ZMM0
store i512 %ZMM1_19, <16 x float>* %ZMM1
br i1 %CC_BE_018, label  %bb_4006AB, label  %bb_10
; <label>:bb_10
%RBP_10 = %RBP
%RAX_12 = %RAX
%EAX_8 = %RAX
%486 = and i32 %EAX_8, -256
%EAX_9 = or i32 %486, 1
%487 = and i64 %RAX_12, -256
%RAX_13 = or i64 %487, 1
%RIP_88 = 4195980
%EIP_76 = 4195980
%488 = %RSP_7 -8
store i64 4195980, i64* %488
%ESP_5 = %RSP -8
store i32 %EAX_9, i32* %EAX
store i32 4196192, i32* %EDI
store i32 %EIP_76, i32* %EIP
store i32 %ESP_5, i32* %ESP
store i64 %RAX_13, i64* %RAX
store i64 %RBP_10, i64* %RBP
store i64 4196192, i64* %RDI
store i64 %RIP_88, i64* %RIP
store i64 %RSP_8, i64* %RSP
store i128 %XMM0_11, <4 x float>* %XMM0
store i256 %YMM0_11, <8 x float>* %YMM0
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
%RBP_11 = %RBP
%552 = %RBP_11 -64
store i128 %XMM1_22, double* %552, align 1
%RAX_14 = %RAX
%EAX_10 = %RAX
%554 = %RBP_11 -68
store i32 %EAX_10, i32* %554, align 1
store i32 %EAX_10, i32* %EAX
store i32 4195942, i32* %EIP
store i64 %RAX_14, i64* %RAX
store i64 %RBP_11, i64* %RBP
store i64 4195942, i64* %RIP
store i128 %XMM0_13, <4 x float>* %XMM0
store i128 %XMM1_22, <4 x float>* %XMM1
store i256 %YMM0_13, <8 x float>* %YMM0
store i256 %YMM1_22, <8 x float>* %YMM1
store i512 %ZMM0_13, <16 x float>* %ZMM0
store i512 %ZMM1_22, <16 x float>* %ZMM1
br label  %bb_9
; <label>:bb_11
%RBP_12 = %RBP
%RIP_99 = 4196021
%EIP_85 = 4196021
%574 = %RSP_9 -8
store i64 4196021, i64* %574
%ESP_6 = %RSP -8
store i32 %EIP_85, i32* %EIP
store i32 %ESP_6, i32* %ESP
store i64 %RBP_12, i64* %RBP
store i64 %RIP_99, i64* %RIP
store i64 %RSP_10, i64* %RSP
store i128 %XMM0_15, <4 x float>* %XMM0
store i256 %YMM0_15, <8 x float>* %YMM0
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
%ZMM0_16 = %ZMM0
%XMM0_16 = %ZMM0
%EAX_11 = %ZMM0
%RAX_16 = %ZMM0
%RIP_102 = %RIP +7
%EIP_87 = %RIP +7
%RBP_13 = %RBP
%603 = %RBP_13 -4
store i32 %EAX_11, i32* %603, align 1
store i32 %EAX_11, i32* %EAX
store i32 %EIP_87, i32* %EIP
store i64 %RAX_16, i64* %RAX
store i64 %RBP_13, i64* %RBP
store i64 %RIP_102, i64* %RIP
store i128 %XMM0_16, <4 x float>* %XMM0
store <16 x float> %ZMM0_16, <16 x float>* %ZMM0
br label  %bb_12
; <label>:bb_12
%EAX_3 = [ %RBP -4 ]
%RAX_5 = %RBP -4
%RBP_4 = [ %RSP +96 ]
%EBP_1 = %RSP +96
%RIP_48 = [ %RSP +88 ]
%ESP_4 = %RSP +96
%EIP_44 = %RSP +88
store i32 %EAX_3, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %EIP_44, i32* %EIP
store i32 %ESP_4, i32* %ESP
store i64 %RAX_5, i64* %RAX
store i64 %RBP_4, i64* %RBP
store i64 %RIP_48, i64* %RIP
store i64 %RSP_6, i64* %RSP
br label %exit_fn_400550

define void @fn_400410(%regset* noalias nocapture) {
entry_fn_400410:
%RIP_init = %regset*
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %regset*
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
br label  %bb_13
exit_fn_400410:                                   ; preds =  %bb_13
%1 = load i64, i64* %RIP
store i64 %1, i64* %RIP_ptr
ret void
; <label>:bb_13
%RIP_2 = 6295576
%EIP_1 = 6295576
%5 = call i64 @llvm.dc.translate.at(i8* %RIP_2)
store i32 %EIP_1, i32* %EIP
store i64 %RIP_2, i64* %RIP
%7 = load i64, i64* %RIP
store i64 %7, i64* %RIP_ptr
call void %5(%regset* %0)
%8 = load i64, i64* %RIP_ptr
store i64 %8, i64* %RIP
br label %exit_fn_400410

define void @fn_400530(%regset* noalias nocapture) {
entry_fn_400530:
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
%ZMM1_init = %regset*
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%XMM1_init = %regset*
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%YMM1_init = %regset*
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%ZMM0_init = %regset*
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%XMM0_init = %regset*
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%YMM0_init = %regset*
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
br label  %bb_14
exit_fn_400530:                                   ; preds =  %bb_14
%9 = load i64, i64* %RBP
store i64 %9, i64* %RBP_ptr
%10 = load i64, i64* %RIP
store i64 %10, i64* %RIP_ptr
%11 = load i64, i64* %RSP
store i64 %11, i64* %RSP_ptr
%12 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %12, <16 x float>* %ZMM0_ptr
%13 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %13, <16 x float>* %ZMM1_ptr
ret void
; <label>:bb_14
%XMM1_1 = %21
%XMM0_0 = %ZMM0
%32 = %RSP_1 -8
store i128 %XMM0_0, double* %32, align 1
%XMM0_1 = %21
%RBP_1 = [ %RSP +8 ]
%EBP_1 = %RSP +8
%RIP_8 = %RSP
%ESP_2 = %RSP +8
%EIP_7 = %RSP
store i32 %EBP_1, i32* %EBP
store i32 %EIP_7, i32* %EIP
store i32 %ESP_2, i32* %ESP
store i64 %RBP_1, i64* %RBP
store i64 %RIP_8, i64* %RIP
store i64 %RSP_3, i64* %RSP
store i128 %XMM0_1, <4 x float>* %XMM0
store i128 %XMM1_1, <4 x float>* %XMM1
store i256 %YMM0_1, <8 x float>* %YMM0
store i256 %YMM1_1, <8 x float>* %YMM1
store i512 %ZMM0_1, <16 x float>* %ZMM0
store i512 %ZMM1_1, <16 x float>* %ZMM1
br label %exit_fn_400530

