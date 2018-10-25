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
%21 = %RSP-8
store i64 %RBP, i64* %RSP-8, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195460
%EIP_1 = 4195460
%EBP_0 = %RSP-8
%RIP_3 = 4195468
%EIP_2 = 4195468
%23 = 4195800
%24 = load double, double* 4195800, align 1
%25 = %24
%ZMM0_0 = %ZMM0
%26 = %ZMM0
%XMM0_0 = %ZMM0
%XMM0_1 = %24
%29 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %24
%32 = %ZMM0
%ZMM0_1 = %24
%RIP_4 = 4195476
%EIP_3 = 4195476
%36 = 4195808
%37 = load double, double* 4195808, align 1
%38 = %37
%ZMM1_0 = %ZMM1
%39 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = %37
%42 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = %37
%45 = %ZMM1
%ZMM1_1 = %37
%RIP_5 = 4195483
%EIP_4 = 4195483
%49 = %RSP-12
store i32 0, i32* %RSP-12, align 1
%RIP_6 = 4195488
%EIP_5 = 4195488
%50 = %37
%51 = %37
%53 = %RSP-24
store double %37, double* %RSP-24, align 1
%RIP_7 = 4195493
%EIP_6 = 4195493
%54 = %24
%55 = %24
%57 = %RSP-32
store double %24, double* %RSP-32, align 1
%RIP_8 = 4195498
%EIP_7 = 4195498
%59 = %RSP-24
%60 = load double, double* %RSP-24, align 1
%61 = %60
%XMM0_2 = %60
%YMM0_2 = %60
%ZMM0_2 = %60
%RIP_9 = 4195503
%EIP_8 = 4195503
%68 = %60
%69 = %60
%71 = %RSP-32
%72 = load double, double* %RSP-32, align 1
%ZF_0 = fcmp ueq double %60, %72
%PF_0 = fcmp uno double %60, %72
%CF_0 = fcmp ult double %60, %72
%73 = %CF_0
%74 = shl i32 %CF_0, 0
%76 = %PF_0
%77 = shl i32 %PF_0, 2
%79 = false
%80 = shl i32 false, 4
%82 = %ZF_0
%83 = shl i32 %ZF_0, 6
%85 = false
%86 = shl i32 false, 7
%88 = false
%89 = shl i32 false, 11
%RIP_10 = 4195509
%EIP_9 = 4195509
%CC_NE_0 = xor i1 %ZF_0, true
store i32 %RSP-8, i32* %EBP
store i32 4195527, i32* %EIP
store i32 %RSP-8, i32* %ESP
store i64 %RSP-8, i64* %RBP
store i64 4195527, i64* %RIP
store i64 %RSP-8, i64* %RSP
%90 = %60
store <4 x float> %60, <4 x float>* %XMM0
%91 = %37
store <4 x float> %37, <4 x float>* %XMM1
%92 = %60
store <8 x float> %60, <8 x float>* %YMM0
%93 = %37
store <8 x float> %37, <8 x float>* %YMM1
%94 = %60
store <16 x float> %60, <16 x float>* %ZMM0
%95 = %37
store <16 x float> %37, <16 x float>* %ZMM1
br i1 %CC_NE_0, label %bb_4004C7, label %bb_4004B5
bb_4004B5: ; preds = %bb_400480
%RIP_13 = 4195515
%EIP_11 = 4195515
store i32 4195527, i32* %EIP
store i64 4195527, i64* %RIP
bb_4004BB: ; preds = %bb_4004B5
%RIP_22 = 4195522
%EIP_18 = 4195522
%RBP_2 = %RSP-8
%98 = %RSP-36
store i32 0, i32* %RSP-36, align 1
%RIP_23 = 4195527
%EIP_19 = 4195527
store i32 4195571, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195571, i64* %RIP
br label %bb_4004F3
bb_4004C7: ; preds = %bb_4004B5, %bb_400480
%RIP_16 = 4195532
%EIP_13 = 4195532
%RBP_1 = %RSP-8
%100 = %RSP-24
%101 = load double, double* %RSP-24, align 1
%102 = %101
%ZMM0_3 = %ZMM0_2
%103 = %ZMM0_2
%XMM0_3 = %ZMM0_2
%XMM0_4 = %101
%106 = %ZMM0_2
%YMM0_3 = %ZMM0_2
%YMM0_4 = %101
%109 = %ZMM0_2
%ZMM0_4 = %101
%RIP_17 = 4195537
%EIP_14 = 4195537
%113 = %RSP-32
%114 = load double, double* %RSP-32, align 1
%115 = %114
%ZMM1_2 = %ZMM1_1
%116 = %ZMM1_1
%XMM1_2 = %ZMM1_1
%XMM1_3 = %114
%119 = %ZMM1_1
%YMM1_2 = %ZMM1_1
%YMM1_3 = %114
%122 = %ZMM1_1
%ZMM1_3 = %114
%RIP_18 = 4195541
%EIP_15 = 4195541
%125 = %114
%126 = %114
%127 = %101
%128 = %101
%ZF_02 = fcmp ueq double %114, %101
%PF_03 = fcmp uno double %114, %101
%CF_04 = fcmp ult double %114, %101
%129 = %CF_04
%130 = shl i32 %CF_04, 0
%132 = %PF_03
%133 = shl i32 %PF_03, 2
%135 = false
%136 = shl i32 false, 4
%138 = %ZF_02
%139 = shl i32 %ZF_02, 6
%141 = false
%142 = shl i32 false, 7
%144 = false
%145 = shl i32 false, 11
%RIP_19 = 4195547
%EIP_16 = 4195547
%CC_BE_0 = or i1 %CF_04, %ZF_02
store i32 4195559, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195559, i64* %RIP
%146 = %101
store <4 x float> %101, <4 x float>* %XMM0
%147 = %114
store <4 x float> %114, <4 x float>* %XMM1
%148 = %101
store <8 x float> %101, <8 x float>* %YMM0
%149 = %114
store <8 x float> %114, <8 x float>* %YMM1
%150 = %101
store <16 x float> %101, <16 x float>* %ZMM0
%151 = %114
store <16 x float> %114, <16 x float>* %ZMM1
br i1 %CC_BE_0, label %bb_4004E7, label %bb_4004DB
bb_4004DB: ; preds = %bb_4004C7
%RIP_26 = 4195554
%EIP_21 = 4195554
%RBP_3 = %RSP-8
%153 = %RSP-36
store i32 -1, i32* %RSP-36, align 1
%RIP_27 = 4195559
%EIP_22 = 4195559
store i32 4195566, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195566, i64* %RIP
br label %bb_4004EE
bb_4004E7: ; preds = %bb_4004C7
%RIP_30 = 4195566
%EIP_24 = 4195566
%RBP_4 = %RSP-8
%155 = %RSP-36
store i32 1, i32* %RSP-36, align 1
store i32 4195566, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195566, i64* %RIP
br label %bb_4004EE
bb_4004EE: ; preds = %bb_4004E7, %bb_4004DB
%RIP_40 = 4195571
%EIP_32 = 4195571
store i32 4195571, i32* %EIP
store i64 4195571, i64* %RIP
br label %bb_4004F3
bb_4004F3: ; preds = %bb_4004EE, %bb_4004BB
%RIP_32 = 4195574
%EIP_25 = 4195574
%RBP_5 = %RSP-8
%157 = %RSP-36
%EAX_0 = %RSP-36
%RAX_0 = %RAX
%RAX_1 = %RSP-36
%158 = lshr i32 %RSP-36, 8
%RIP_33 = 4195576
%EIP_26 = 4195576
%RCX_0 = %RCX
%RCX_1 = %RSP-36
%159 = lshr i32 %RSP-36, 8
%RIP_34 = 4195579
%EIP_27 = 4195579
%ECX_0 = %RSP-35
%RCX_2 = %RSP-35
%160 = lshr i32 %RSP-35, 8
%RIP_35 = 4195582
%EIP_28 = 4195582
%162 = %RSP-44
store i32 %RSP-36, i32* %RSP-44, align 1
%RIP_36 = 4195585
%EIP_29 = 4195585
%164 = %RSP-48
store i32 %RSP-35, i32* %RSP-48, align 1
%RIP_37 = 4195591
%EIP_30 = 4195591
%ZF_05 = icmp eq i32 %RSP-35, 0
%SF_0 = icmp slt i32 %RSP-35, 0
%165 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %RSP-36, i32 -1)
%OF_0 = extractvalue { i32, i1 } %165, 1
%166 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %RSP-36, i32 -1)
%CF_06 = extractvalue { i32, i1 } %166, 1
%167 = %RSP-35
%168 = call i8 @llvm.ctpop.i8(i8 %RSP-35)
%169 = %168
%PF_07 = icmp eq i1 %168, false
%170 = %166
%171 = shl i32 %166, 0
%173 = %PF_07
%174 = shl i32 %PF_07, 2
%176 = false
%177 = shl i32 false, 4
%179 = %ZF_05
%180 = shl i32 %ZF_05, 6
%182 = %SF_0
%183 = shl i32 %SF_0, 7
%185 = %165
%186 = shl i32 %165, 11
store i32 %RSP-36, i32* %EAX
store i32 %RSP-35, i32* %ECX
store i32 4195616, i32* %EIP
store i64 %RSP-36, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 %RSP-35, i64* %RCX
store i64 4195616, i64* %RIP
bb_400507: ; preds = %bb_4004F3
%RIP_43 = 4195596
%EIP_34 = 4195596
store i32 4195596, i32* %EIP
store i64 4195596, i64* %RIP
br label %bb_40050C
bb_40050C: ; preds = %bb_400507
%RIP_50 = 4195599
%EIP_39 = 4195599
%RBP_7 = %RSP-8
%189 = %RSP-44
%EAX_1 = %RSP-44
%RAX_2 = %RSP-36
%RAX_3 = %RSP-44
%190 = lshr i32 %RSP-44, 8
%RIP_51 = 4195602
%EIP_40 = 4195602
%EAX_2 = %RSP-45
%RAX_4 = %RSP-45
%191 = lshr i32 %RSP-45, 8
%RIP_52 = 4195605
%EIP_41 = 4195605
%193 = %RSP-52
store i32 %RSP-45, i32* %RSP-52, align 1
%RIP_53 = 4195611
%EIP_42 = 4195611
%ZF_08 = icmp eq i32 %RSP-45, 0
%SF_09 = icmp slt i32 %RSP-45, 0
%194 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %RSP-44, i32 1)
%OF_010 = extractvalue { i32, i1 } %194, 1
%195 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %RSP-44, i32 1)
%CF_011 = extractvalue { i32, i1 } %195, 1
%196 = %RSP-45
%197 = call i8 @llvm.ctpop.i8(i8 %RSP-45)
%198 = %197
%PF_012 = icmp eq i1 %197, false
%199 = %195
%200 = shl i32 %195, 0
%202 = %PF_012
%203 = shl i32 %PF_012, 2
%205 = false
%206 = shl i32 false, 4
%208 = %ZF_08
%209 = shl i32 %ZF_08, 6
%211 = %SF_09
%212 = shl i32 %SF_09, 7
%214 = %194
%215 = shl i32 %194, 11
store i32 %RSP-45, i32* %EAX
store i32 4195628, i32* %EIP
store i64 %RSP-45, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 4195628, i64* %RIP
bb_40051B: ; preds = %bb_40050C
%RIP_61 = 4195616
%EIP_48 = 4195616
store i32 4195640, i32* %EIP
store i64 4195640, i64* %RIP
br label %bb_400538
bb_400520: ; preds = %bb_4004F3
%RIP_46 = 4195623
%EIP_36 = 4195623
%RBP_6 = %RSP-8
%218 = %RSP-40
store i32 -1, i32* %RSP-40, align 1
%RIP_47 = 4195628
%EIP_37 = 4195628
store i32 4195647, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_40052C: ; preds = %bb_40050C
%RIP_64 = 4195635
%EIP_50 = 4195635
%RBP_10 = %RSP-8
%220 = %RSP-40
store i32 1, i32* %RSP-40, align 1
%RIP_65 = 4195640
%EIP_51 = 4195640
store i32 4195647, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_400538: ; preds = %bb_40051B
%RIP_68 = 4195647
%EIP_53 = 4195647
%RBP_11 = %RSP-8
%222 = %RSP-40
store i32 0, i32* %RSP-40, align 1
store i32 4195647, i32* %EIP
store i64 %RSP-8, i64* %RBP
store i64 4195647, i64* %RIP
br label %bb_40053F
bb_40053F: ; preds = %bb_400538, %bb_40052C, %bb_400520
%RIP_56 = 4195650
%EIP_44 = 4195650
%RBP_8 = %RSP-8
%224 = %RSP-40
%EAX_3 = %RSP-40
%RAX_5 = %RSP-45
%RAX_6 = %RSP-40
%225 = lshr i32 %RSP-40, 8
%RIP_57 = 4195651
%EIP_45 = 4195651
%RSP_2 = %RSP-8
%RSP_3 = %RSP
%ESP_1 = %RSP
%227 = %RSP-8
%RBP_9 = %RSP-8
%EBP_1 = %RSP-8
%RIP_58 = 4195652
%EIP_46 = 4195652
%RSP_4 = %RSP+8
%228 = %RSP
%RIP_59 = %RSP
%ESP_2 = %RSP+8
%EIP_47 = %RSP
store i32 %RSP-40, i32* %EAX
store i32 %RSP-8, i32* %EBP
store i32 %RSP, i32* %EIP
store i32 %RSP+8, i32* %ESP
store i64 %RSP-40, i64* %RAX
store i64 %RSP-8, i64* %RBP
store i64 %RSP, i64* %RIP
store i64 %RSP+8, i64* %RSP
br label %exit_fn_400480
}

