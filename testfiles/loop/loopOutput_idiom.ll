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
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%9 = lshr i64 %RAX_init, 8
br label %bb_400480
exit_fn_400480:                                   ; preds = %bb_400503
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
bb_400480:                                        ; preds = %entry_fn_400480
%RIP_1 = 4195457
%EIP_0 = 4195457
%RBP_0 = %RBP
%RSP_0 = %RSP
%19 = %RSP_0-8
store i64 %RBP_0, i64* %19, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195460
%EIP_1 = 4195460
%EBP_0 = %RSP-8
%RIP_3 = 4195468
%EIP_2 = 4195468
%21 = %RIP_3+268
%22 = load double, double* %21, align 1
%23 = bitcast double %22 to i64
%ZMM0_0 = %ZMM0
%24 = bitcast <16 x float> %ZMM0_0 to i512
%XMM0_0 = %ZMM0
%XMM0_1 = %XMM0_0 : %23
%27 = bitcast <16 x float> %ZMM0_0 to i512
%YMM0_0 = %ZMM0
%YMM0_1 = %YMM0_0 : %XMM0_1
%30 = bitcast <16 x float> %ZMM0_0 to i512
%ZMM0_1 = %30 : %XMM0_1
%RIP_4 = 4195476
%EIP_3 = 4195476
%34 = %RIP_4+268
%35 = load double, double* %34, align 1
%36 = bitcast double %35 to i64
%ZMM1_0 = %ZMM1
%37 = bitcast <16 x float> %ZMM1_0 to i512
%XMM1_0 = %ZMM1
%XMM1_1 = %XMM1_0 : %36
%40 = bitcast <16 x float> %ZMM1_0 to i512
%YMM1_0 = %ZMM1
%YMM1_1 = %YMM1_0 : %XMM1_1
%43 = bitcast <16 x float> %ZMM1_0 to i512
%ZMM1_1 = %43 : %XMM1_1
%RIP_5 = 4195483
%EIP_4 = 4195483
%47 = %RSP_1-4
store i32 0, i32* %47, align 1
%RIP_6 = 4195490
%EIP_5 = 4195490
%49 = %RSP_1-8
store i32 5, i32* %49, align 1
%RIP_7 = 4195497
%EIP_6 = 4195497
%51 = %RSP_1-12
store i32 625, i32* %51, align 1
%RIP_8 = 4195502
%EIP_7 = 4195502
%52 = trunc i128 %XMM1_1 to i64
%53 = bitcast i64 %52 to double
%55 = %RSP_1-24
store double %53, double* %55, align 1
%RIP_9 = 4195507
%EIP_8 = 4195507
%56 = trunc i128 %XMM0_1 to i64
%57 = bitcast i64 %56 to double
%59 = %RSP_1-32
store double %57, double* %59, align 1
store i32 %EBP_0, i32* %EBP
store i32 %EIP_8, i32* %EIP
store i32 %ESP_0, i32* %ESP
store i64 %RSP_1, i64* %RBP
store i64 %RIP_9, i64* %RIP
store i64 %RSP_1, i64* %RSP
%60 = bitcast i128 %XMM0_1 to <4 x float>
store <4 x float> %60, <4 x float>* %XMM0
%61 = bitcast i128 %XMM1_1 to <4 x float>
store <4 x float> %61, <4 x float>* %XMM1
%62 = bitcast i256 %YMM0_1 to <8 x float>
store <8 x float> %62, <8 x float>* %YMM0
%63 = bitcast i256 %YMM1_1 to <8 x float>
store <8 x float> %63, <8 x float>* %YMM1
%64 = bitcast i512 %ZMM0_1 to <16 x float>
store <16 x float> %64, <16 x float>* %ZMM0
%65 = bitcast i512 %ZMM1_1 to <16 x float>
store <16 x float> %65, <16 x float>* %ZMM1
br label %bb_4004B3
bb_4004B3:                                        ; preds = %bb_4004BF, %bb_400480
%RIP_19 = 4195510
%EIP_15 = 4195510
%RBP_2 = %RSP-8
%67 = %RBP_2-8
%EAX_1 = %RSP-16
%RAX_2 = %RAX
%RAX_3 = %RSP-16
%68 = lshr i32 %EAX_1, 8
%RIP_20 = 4195513
%EIP_16 = 4195513
%70 = %RBP_2-12
%71 = load i32, i32* %70, align 1
%CC_A_0 = icmp ugt i32 %EAX_1, %71
%CC_AE_0 = icmp uge i32 %EAX_1, %71
%CC_B_0 = icmp ult i32 %EAX_1, %71
%CC_BE_0 = icmp ule i32 %EAX_1, %71
%CC_L_0 = icmp slt i32 %EAX_1, %71
%CC_LE_0 = icmp sle i32 %EAX_1, %71
%CC_G_0 = icmp sgt i32 %EAX_1, %71
%CC_GE_0 = icmp sge i32 %EAX_1, %71
%CC_E_0 = icmp eq i32 %EAX_1, %71
%CC_NE_0 = icmp ne i32 %EAX_1, %71
%72 = %71-%EAX_1
%ZF_01 = icmp eq i32 %72, 0
%SF_02 = icmp slt i32 %72, 0
%73 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_1, i32 %71)
%OF_0 = extractvalue { i32, i1 } %73, 1
%74 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_1, i32 %71)
%CF_0 = extractvalue { i32, i1 } %74, 1
%75 = trunc i32 %72 to i8
%76 = call i8 @llvm.ctpop.i8(i8 %75)
%77 = trunc i8 %76 to i1
%PF_03 = icmp eq i1 %77, false
%78 = zext i1 %CF_0 to i32
%79 = shl i32 %78, 0
%81 = zext i1 %PF_03 to i32
%82 = shl i32 %81, 2
%84 = zext i1 false to i32
%85 = shl i32 %84, 4
%87 = zext i1 %ZF_01 to i32
%88 = shl i32 %87, 6
%90 = zext i1 %SF_02 to i32
%91 = shl i32 %90, 7
%93 = zext i1 %OF_0 to i32
%94 = shl i32 %93, 11
%RIP_21 = 4195519
%EIP_17 = 4195519
store i32 %EAX_1, i32* %EAX
store i32 4195531, i32* %EIP
store i64 %RAX_3, i64* %RAX
store i64 %RBP_2, i64* %RBP
store i64 4195531, i64* %RIP
br i1 %CC_GE_0, label %bb_4004CB, label %bb_4004BF
bb_4004BF:                                        ; preds = %bb_4004B3
%RIP_11 = 4195523
%EIP_9 = 4195523
%RBP_1 = %RSP-8
%96 = %RBP_1-8
%97 = load i32, i32* %96, align 1
%EAX_0 = %RSP-11
%RAX_0 = %RSP-16
%RAX_1 = %EAX_0
%98 = lshr i32 %EAX_0, 8
%RIP_12 = 4195526
%EIP_10 = 4195526
%100 = %RBP_1-8
store i32 %EAX_0, i32* %100, align 1
%RIP_13 = 4195531
%EIP_11 = 4195531
%ZF_0 = icmp eq i32 %EAX_0, 0
%SF_0 = icmp slt i32 %EAX_0, 0
%101 = trunc i32 %EAX_0 to i8
%102 = call i8 @llvm.ctpop.i8(i8 %101)
%103 = trunc i8 %102 to i1
%PF_0 = icmp eq i1 %103, false
%104 = zext i1 false to i32
%105 = shl i32 %104, 0
%107 = zext i1 %PF_0 to i32
%108 = shl i32 %107, 2
%110 = zext i1 false to i32
%111 = shl i32 %110, 4
%113 = zext i1 %ZF_0 to i32
%114 = shl i32 %113, 6
%116 = zext i1 %SF_0 to i32
%117 = shl i32 %116, 7
%119 = zext i1 false to i32
%120 = shl i32 %119, 11
store i32 %EAX_0, i32* %EAX
store i32 4195507, i32* %EIP
store i64 %RAX_1, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 4195507, i64* %RIP
br label %bb_4004B3
bb_4004CB:                                        ; preds = %bb_4004B3
%RIP_16 = 4195536
%EIP_13 = 4195536
store i32 4195536, i32* %EIP
store i64 4195536, i64* %RIP
br label %bb_4004D0
bb_4004D0:                                        ; preds = %bb_4004E0, %bb_4004CB
%RIP_24 = 4195541
%EIP_19 = 4195541
%RBP_3 = %RSP-8
%122 = %RBP_3-24
%123 = load double, double* %122, align 1
%124 = bitcast double %123 to i64
%ZMM0_2 = %ZMM0_1
%125 = bitcast <16 x float> %ZMM0_2 to i512
%XMM0_2 = %ZMM0_1
%XMM0_3 = %XMM0_2 : %124
%128 = bitcast <16 x float> %ZMM0_2 to i512
%YMM0_2 = %ZMM0_1
%YMM0_3 = %YMM0_2 : %XMM0_3
%131 = bitcast <16 x float> %ZMM0_2 to i512
%ZMM0_3 = %131 : %XMM0_3
%RIP_25 = 4195546
%EIP_20 = 4195546
%134 = trunc i128 %XMM0_3 to i64
%135 = bitcast i64 %134 to double
%137 = %RBP_3-32
%138 = load double, double* %137, align 1
%ZF_04 = fcmp ueq double %135, %138
%PF_05 = fcmp uno double %135, %138
%CF_06 = fcmp ult double %135, %138
%139 = zext i1 %CF_06 to i32
%140 = shl i32 %139, 0
%142 = zext i1 %PF_05 to i32
%143 = shl i32 %142, 2
%145 = zext i1 false to i32
%146 = shl i32 %145, 4
%148 = zext i1 %ZF_04 to i32
%149 = shl i32 %148, 6
%151 = zext i1 false to i32
%152 = shl i32 %151, 7
%154 = zext i1 false to i32
%155 = shl i32 %154, 11
%RIP_26 = 4195552
%EIP_21 = 4195552
store i32 4195587, i32* %EIP
store i64 %RBP_3, i64* %RBP
store i64 4195587, i64* %RIP
%156 = bitcast i128 %XMM0_3 to <4 x float>
store <4 x float> %156, <4 x float>* %XMM0
%157 = bitcast i256 %YMM0_3 to <8 x float>
store <8 x float> %157, <8 x float>* %YMM0
%158 = bitcast i512 %ZMM0_3 to <16 x float>
store <16 x float> %158, <16 x float>* %ZMM0
br i1 %CF_06, label %bb_400503, label %bb_4004E0
bb_4004E0:                                        ; preds = %bb_4004D0
%RIP_29 = 4195557
%EIP_23 = 4195557
%RBP_4 = %RSP-8
%160 = %RBP_4-24
%161 = load double, double* %160, align 1
%162 = bitcast double %161 to i64
%ZMM0_4 = %ZMM0_3
%163 = bitcast <16 x float> %ZMM0_4 to i512
%XMM0_4 = %ZMM0_3
%XMM0_5 = %XMM0_4 : %162
%166 = bitcast <16 x float> %ZMM0_4 to i512
%YMM0_4 = %ZMM0_3
%YMM0_5 = %YMM0_4 : %XMM0_5
%169 = bitcast <16 x float> %ZMM0_4 to i512
%ZMM0_5 = %169 : %XMM0_5
%RIP_30 = 4195562
%EIP_24 = 4195562
%172 = trunc i128 %XMM0_5 to i64
%173 = bitcast i64 %172 to double
%175 = %RBP_4-32
%176 = load double, double* %175, align 1
%177 = fdiv double %173, %176
%178 = bitcast double %177 to i64
%XMM0_6 = %XMM0_5 : %178
%YMM0_6 = %YMM0_5 : %XMM0_6
%ZMM0_6 = %ZMM0_5 : %XMM0_6
%RIP_31 = 4195567
%EIP_25 = 4195567
%185 = trunc i128 %XMM0_6 to i64
%186 = bitcast i64 %185 to double
%188 = %RBP_4-40
store double %186, double* %188, align 1
%RIP_32 = 4195572
%EIP_26 = 4195572
%190 = %RBP_4-40
%191 = load double, double* %190, align 1
%192 = bitcast double %191 to i64
%XMM0_7 = %XMM0_6 : %192
%YMM0_7 = %YMM0_6 : %XMM0_7
%ZMM0_7 = %ZMM0_6 : %XMM0_7
%RIP_33 = 4195577
%EIP_27 = 4195577
%199 = trunc i128 %XMM0_7 to i64
%200 = bitcast i64 %199 to double
%202 = %RBP_4-32
%203 = load double, double* %202, align 1
%204 = fadd double %200, %203
%205 = bitcast double %204 to i64
%XMM0_8 = %XMM0_7 : %205
%YMM0_8 = %YMM0_7 : %XMM0_8
%ZMM0_8 = %ZMM0_7 : %XMM0_8
%RIP_34 = 4195582
%EIP_28 = 4195582
%212 = trunc i128 %XMM0_8 to i64
%213 = bitcast i64 %212 to double
%215 = %RBP_4-32
store double %213, double* %215, align 1
%RIP_35 = 4195587
%EIP_29 = 4195587
store i32 4195536, i32* %EIP
store i64 %RBP_4, i64* %RBP
store i64 4195536, i64* %RIP
%216 = bitcast i128 %XMM0_8 to <4 x float>
store <4 x float> %216, <4 x float>* %XMM0
%217 = bitcast i256 %YMM0_8 to <8 x float>
store <8 x float> %217, <8 x float>* %YMM0
%218 = bitcast i512 %ZMM0_8 to <16 x float>
store <16 x float> %218, <16 x float>* %ZMM0
br label %bb_4004D0
bb_400503:                                        ; preds = %bb_4004D0
%RIP_38 = 4195589
%EIP_31 = 4195589
%RAX_4 = %EAX_0
%EAX_2 = %EAX_0
%EAX_3 = xor i32 %EAX_2, %EAX_2
%RAX_5 = %EAX_3
%219 = lshr i32 %EAX_3, 8
%RIP_39 = 4195590
%EIP_32 = 4195590
%RSP_2 = %RSP-8
%RSP_3 = %RSP
%ESP_1 = %RSP
%221 = %RSP_3-8
%RBP_5 = %RSP-8
%EBP_1 = %RSP-8
%RIP_40 = 4195591
%EIP_33 = 4195591
%RSP_4 = %RSP+8
%222 = inttoptr i64 %RSP_3 to i64*
%RIP_41 = %RSP
%ESP_2 = %RSP+8
%EIP_34 = %RSP
%ZF_07 = icmp eq i32 %EAX_3, 0
%SF_08 = icmp slt i32 %EAX_3, 0
%223 = trunc i32 %EAX_3 to i8
%224 = call i8 @llvm.ctpop.i8(i8 %223)
%225 = trunc i8 %224 to i1
%PF_09 = icmp eq i1 %225, false
%226 = zext i1 false to i32
%227 = shl i32 %226, 0
%229 = zext i1 %PF_09 to i32
%230 = shl i32 %229, 2
%232 = zext i1 false to i32
%233 = shl i32 %232, 4
%235 = zext i1 %ZF_07 to i32
%236 = shl i32 %235, 6
%238 = zext i1 %SF_08 to i32
%239 = shl i32 %238, 7
%241 = zext i1 false to i32
%242 = shl i32 %241, 11
store i32 %EAX_3, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %EIP_34, i32* %EIP
store i32 %ESP_2, i32* %ESP
store i64 %RAX_5, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 %RIP_41, i64* %RIP
store i64 %RSP_4, i64* %RSP
br label %exit_fn_400480
}

