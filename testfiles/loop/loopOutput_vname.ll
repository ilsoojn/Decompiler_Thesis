define void @fn_400480(%regset* noalias nocapture) {
%g = alloca double, align 8
%f = alloca double, align 8
%e = alloca double, align 8
%d = alloca i32, align 4
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
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
%1 = %ZMM0_init
%2 = %ZMM0_init
%XMM0_init = %ZMM0_ptr
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%3 = %ZMM0_init
%4 = %ZMM0_init
%YMM0_init = %ZMM0_ptr
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%5 = %ZMM1_init
%6 = %ZMM1_init
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%7 = %ZMM1_init
%8 = %ZMM1_init
%YMM1_init = %ZMM1_ptr
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%9 = lshr i64 %RAX_init, 8
br label %bb_400480
exit_fn_400480: ; preds = %bb_400503
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
bb_400480: ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%19 = %a
store i64 %RBP, i64* %a, align 1
%RSP_1 = %a
%ESP_0 = %a
%RIP_2 = 4195460
%EIP_1 = 4195460
%EBP_0 = %a
%RIP_3 = 4195468
%EIP_2 = 4195468
%21 = 4195736
%22 = load double, double* 4195736, align 1
%23 = %22
%ZMM0_0 = %ZMM0
%24 = %ZMM0
%XMM0_0 = %ZMM0
%XMM0_1 = %22
%27 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %22
%30 = %ZMM0
%ZMM0_1 = %22
%RIP_4 = 4195476
%EIP_3 = 4195476
%34 = 4195744
%35 = load double, double* 4195744, align 1
%36 = %35
%ZMM1_0 = %ZMM1
%37 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = %35
%40 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = %35
%43 = %ZMM1
%ZMM1_1 = %35
%RIP_5 = 4195483
%EIP_4 = 4195483
%47 = %b
store i32 0, i32* %b, align 1
%RIP_6 = 4195490
%EIP_5 = 4195490
%49 = %c
store i32 5, i32* %c, align 1
%RIP_7 = 4195497
%EIP_6 = 4195497
%51 = %d
store i32 625, i32* %d, align 1
%RIP_8 = 4195502
%EIP_7 = 4195502
%52 = %35
%53 = %35
%55 = %e
store double %35, double* %e, align 1
%RIP_9 = 4195507
%EIP_8 = 4195507
%56 = %22
%57 = %22
%59 = %f
store double %22, double* %f, align 1
store i32 %a, i32* %EBP
store i32 4195507, i32* %EIP
store i32 %a, i32* %ESP
store i64 %a, i64* %RBP
store i64 4195507, i64* %RIP
store i64 %a, i64* %RSP
%60 = %22
store <4 x float> %22, <4 x float>* %XMM0
%61 = %35
store <4 x float> %35, <4 x float>* %XMM1
%62 = %22
store <8 x float> %22, <8 x float>* %YMM0
%63 = %35
store <8 x float> %35, <8 x float>* %YMM1
%64 = %22
store <16 x float> %22, <16 x float>* %ZMM0
%65 = %35
store <16 x float> %35, <16 x float>* %ZMM1
br label %bb_4004B3
bb_4004B3: ; preds = %bb_4004BF, %bb_400480
%RIP_19 = 4195510
%EIP_15 = 4195510
%RBP_2 = %a
%67 = %c
%EAX_1 = %c
%RAX_2 = %RAX
%RAX_3 = %c
%68 = lshr i32 %c, 8
%RIP_20 = 4195513
%EIP_16 = 4195513
%70 = %d
%71 = load i32, i32* %d, align 1
%CC_A_0 = icmp ugt i32 %c, %71
%CC_AE_0 = icmp uge i32 %c, %71
%CC_B_0 = icmp ult i32 %c, %71
%CC_BE_0 = icmp ule i32 %c, %71
%CC_L_0 = icmp slt i32 %c, %71
%CC_LE_0 = icmp sle i32 %c, %71
%CC_G_0 = icmp sgt i32 %c, %71
%CC_GE_0 = icmp sge i32 %c, %71
%CC_E_0 = icmp eq i32 %c, %71
%CC_NE_0 = icmp ne i32 %c, %71
%72 = %71-%c
%ZF_01 = icmp eq i32 %71-%c, 0
%SF_02 = icmp slt i32 %71-%c, 0
%73 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %c, i32 %71)
%OF_0 = extractvalue { i32, i1 } %73, 1
%74 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %c, i32 %71)
%CF_0 = extractvalue { i32, i1 } %74, 1
%75 = %71-%c
%76 = call i8 @llvm.ctpop.i8(i8 %71-%c)
%77 = %76
%PF_03 = icmp eq i1 %76, false
%78 = %74
%79 = shl i32 %74, 0
%81 = %PF_03
%82 = shl i32 %PF_03, 2
%84 = false
%85 = shl i32 false, 4
%87 = %ZF_01
%88 = shl i32 %ZF_01, 6
%90 = %SF_02
%91 = shl i32 %SF_02, 7
%93 = %73
%94 = shl i32 %73, 11
%RIP_21 = 4195519
%EIP_17 = 4195519
store i32 %c, i32* %EAX
store i32 4195531, i32* %EIP
store i64 %c, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195531, i64* %RIP
br i1 %CC_GE_0, label %bb_4004CB, label %bb_4004BF
bb_4004BF: ; preds = %bb_4004B3
%RIP_11 = 4195523
%EIP_9 = 4195523
%RBP_1 = %a
%96 = %c
%97 = load i32, i32* %c, align 1
%EAX_0 = %RSP-11
%RAX_0 = %c
%RAX_1 = %EAX_0
%98 = lshr i32 %RSP-11, 8
%RIP_12 = 4195526
%EIP_10 = 4195526
%100 = %c
store i32 %RSP-11, i32* %c, align 1
%RIP_13 = 4195531
%EIP_11 = 4195531
%ZF_0 = icmp eq i32 %RSP-11, 0
%SF_0 = icmp slt i32 %RSP-11, 0
%101 = %RSP-11
%102 = call i8 @llvm.ctpop.i8(i8 %RSP-11)
%103 = %102
%PF_0 = icmp eq i1 %102, false
%104 = false
%105 = shl i32 false, 0
%107 = %PF_0
%108 = shl i32 %PF_0, 2
%110 = false
%111 = shl i32 false, 4
%113 = %ZF_0
%114 = shl i32 %ZF_0, 6
%116 = %SF_0
%117 = shl i32 %SF_0, 7
%119 = false
%120 = shl i32 false, 11
store i32 %RSP-11, i32* %EAX
store i32 4195507, i32* %EIP
store i64 %EAX_0, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195507, i64* %RIP
br label %bb_4004B3
bb_4004CB: ; preds = %bb_4004B3
%RIP_16 = 4195536
%EIP_13 = 4195536
store i32 4195536, i32* %EIP
store i64 4195536, i64* %RIP
br label %bb_4004D0
bb_4004D0: ; preds = %bb_4004E0, %bb_4004CB
%RIP_24 = 4195541
%EIP_19 = 4195541
%RBP_3 = %a
%122 = %e
%123 = load double, double* %e, align 1
%124 = %123
%ZMM0_2 = %ZMM0_1
%125 = %ZMM0_1
%XMM0_2 = %ZMM0_1
%XMM0_3 = %123
%128 = %ZMM0_1
%YMM0_2 = %ZMM0_1
%YMM0_3 = %123
%131 = %ZMM0_1
%ZMM0_3 = %123
%RIP_25 = 4195546
%EIP_20 = 4195546
%134 = %123
%135 = %123
%137 = %f
%138 = load double, double* %f, align 1
%ZF_04 = fcmp ueq double %123, %138
%PF_05 = fcmp uno double %123, %138
%CF_06 = fcmp ult double %123, %138
%139 = %CF_06
%140 = shl i32 %CF_06, 0
%142 = %PF_05
%143 = shl i32 %PF_05, 2
%145 = false
%146 = shl i32 false, 4
%148 = %ZF_04
%149 = shl i32 %ZF_04, 6
%151 = false
%152 = shl i32 false, 7
%154 = false
%155 = shl i32 false, 11
%RIP_26 = 4195552
%EIP_21 = 4195552
store i32 4195587, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195587, i64* %RIP
%156 = %123
store <4 x float> %123, <4 x float>* %XMM0
%157 = %123
store <8 x float> %123, <8 x float>* %YMM0
%158 = %123
store <16 x float> %123, <16 x float>* %ZMM0
br i1 %CF_06, label %bb_400503, label %bb_4004E0
bb_4004E0: ; preds = %bb_4004D0
%RIP_29 = 4195557
%EIP_23 = 4195557
%RBP_4 = %a
%160 = %e
%161 = load double, double* %e, align 1
%162 = %161
%ZMM0_4 = %ZMM0_3
%163 = %ZMM0_3
%XMM0_4 = %ZMM0_3
%XMM0_5 = %161
%166 = %ZMM0_3
%YMM0_4 = %ZMM0_3
%YMM0_5 = %161
%169 = %ZMM0_3
%ZMM0_5 = %161
%RIP_30 = 4195562
%EIP_24 = 4195562
%172 = %161
%173 = %161
%175 = %f
%176 = load double, double* %f, align 1
%177 = fdiv double %161, %176
%178 = %177
%XMM0_6 = %177
%YMM0_6 = %177
%ZMM0_6 = %177
%RIP_31 = 4195567
%EIP_25 = 4195567
%185 = %177
%186 = %177
%188 = %g
store double %177, double* %g, align 1
%RIP_32 = 4195572
%EIP_26 = 4195572
%190 = %g
%191 = load double, double* %g, align 1
%192 = %191
%XMM0_7 = %191
%YMM0_7 = %191
%ZMM0_7 = %191
%RIP_33 = 4195577
%EIP_27 = 4195577
%199 = %191
%200 = %191
%202 = %f
%203 = load double, double* %f, align 1
%204 = fadd double %191, %203
%205 = %204
%XMM0_8 = %204
%YMM0_8 = %204
%ZMM0_8 = %204
%RIP_34 = 4195582
%EIP_28 = 4195582
%212 = %204
%213 = %204
%215 = %f
store double %204, double* %f, align 1
%RIP_35 = 4195587
%EIP_29 = 4195587
store i32 4195536, i32* %EIP
store i64 %a, i64* %RBP
store i64 4195536, i64* %RIP
%216 = %204
store <4 x float> %204, <4 x float>* %XMM0
%217 = %204
store <8 x float> %204, <8 x float>* %YMM0
%218 = %204
store <16 x float> %204, <16 x float>* %ZMM0
br label %bb_4004D0
bb_400503: ; preds = %bb_4004D0
%RIP_38 = 4195589
%EIP_31 = 4195589
%RAX_4 = %EAX_0
%EAX_2 = %EAX_0
%EAX_3 = xor i32 %EAX_2, %EAX_2
%RAX_5 = %EAX_3
%219 = lshr i32 %EAX_3, 8
%RIP_39 = 4195590
%EIP_32 = 4195590
%RSP_2 = %a
%RSP_3 = %RSP
%ESP_1 = %RSP
%221 = %a
%RBP_5 = %a
%EBP_1 = %a
%RIP_40 = 4195591
%EIP_33 = 4195591
%RSP_4 = %RSP+8
%222 = %RSP
%RIP_41 = %RSP
%ESP_2 = %RSP+8
%EIP_34 = %RSP
%ZF_07 = icmp eq i32 %EAX_3, 0
%SF_08 = icmp slt i32 %EAX_3, 0
%223 = %EAX_3
%224 = call i8 @llvm.ctpop.i8(i8 %EAX_3)
%225 = %224
%PF_09 = icmp eq i1 %224, false
%226 = false
%227 = shl i32 false, 0
%229 = %PF_09
%230 = shl i32 %PF_09, 2
%232 = false
%233 = shl i32 false, 4
%235 = %ZF_07
%236 = shl i32 %ZF_07, 6
%238 = %SF_08
%239 = shl i32 %SF_08, 7
%241 = false
%242 = shl i32 false, 11
store i32 %EAX_3, i32* %EAX
store i32 %a, i32* %EBP
store i32 %RSP, i32* %EIP
store i32 %RSP+8, i32* %ESP
store i64 %EAX_3, i64* %RAX
store i64 %a, i64* %RBP
store i64 %RSP, i64* %RIP
store i64 %RSP+8, i64* %RSP
br label %exit_fn_400480
}

