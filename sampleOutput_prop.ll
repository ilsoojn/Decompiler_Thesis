define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
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
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = %regset*
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = %regset*
%EDI = alloca i32
store i32 %EDI_init, i32* %EDI
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
%RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
%RSI_init = %regset*
%RSI = alloca i64
store i64 %RSI_init, i64* %RSI
%ESI_init = %regset*
%ESI = alloca i32
store i32 %ESI_init, i32* %ESI
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
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %regset*
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %regset*
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %regset*, 8
br label %bb_4004D0
exit_fn_4004D0:                                   ; preds = %bb_4005EB
%13 = load i64, i64* %RAX
store i64 %13, i64* %RAX_ptr
%14 = load i64, i64* %RBP
store i64 %14, i64* %RBP_ptr
%15 = load i64, i64* %RCX
store i64 %15, i64* %RCX_ptr
%16 = load i64, i64* %RDI
store i64 %16, i64* %RDI_ptr
%17 = load i64, i64* %RIP
store i64 %17, i64* %RIP_ptr
%18 = load i64, i64* %RSI
store i64 %18, i64* %RSI_ptr
%19 = load i64, i64* %RSP
store i64 %19, i64* %RSP_ptr
%20 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %20, <16 x float>* %ZMM0_ptr
%21 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %21, <16 x float>* %ZMM1_ptr
ret void
bb_4004D0:                                        ; preds = %entry_fn_4004D0
%RIP_1 = 4195537
%EIP_0 = 4195537
%RBP_0 = %RBP
%RSP_0 = %RSP
%23 = %RSP-8
store i64 %RBP_0, i64* %23, align 1
%RSP_1 = %RSP-8
%ESP_0 = %RSP-8
%RIP_2 = 4195540
%EIP_1 = 4195540
%EBP_0 = %RSP-8
%RIP_3 = 4195544
%EIP_2 = 4195544
%RSP_2 = %RSP-56
%ESP_1 = %RSP-56
%RIP_4 = 4195554
%EIP_3 = 4195554
%RIP_5 = 4195562
%EIP_4 = 4195562
%25 = 4195562+430
%26 = load double, double* %25, align 1
%27 = bitcast double %26 to i64
%ZMM0_0 = %ZMM0
%28 = %ZMM0
%XMM0_0 = %ZMM0
%29 = zext double %26 to i128
%30 = and i128 %ZMM0, -18446744073709551616
%XMM0_1 = or i128 %29, %30
%31 = %ZMM0
%YMM0_0 = %ZMM0
%32 = zext i128 %XMM0_1 to i256
%33 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_1 = or i256 %32, %33
%34 = %ZMM0
%35 = zext i128 %XMM0_1 to i512
%36 = and i512 %34, -340282366920938463463374607431768211456
%ZMM0_1 = or i512 %35, %36
%RIP_6 = 4195569
%EIP_5 = 4195569
%38 = %RSP-8-4
store i32 0, i32* %38, align 1
%RIP_7 = 4195576
%EIP_6 = 4195576
%40 = %RSP-8-8
store i32 5, i32* %40, align 1
%RIP_8 = 4195581
%EIP_7 = 4195581
%41 = trunc i128 %XMM0_1 to i64
%42 = bitcast i128 %XMM0_1 to double
%44 = %RSP-8-16
store i128 %XMM0_1, double* %44, align 1
%RIP_9 = 4195588
%EIP_8 = 4195588
%46 = %RSP-8-20
store i32 100, i32* %46, align 1
%RIP_10 = 4195591
%EIP_9 = 4195591
%48 = %RSP-8-20
%ESI_0 = [ %RSP -28 ]
%RSI_0 = %RSI
%RSI_1 = %RSP-28
%RIP_11 = 4195596
%EIP_10 = 4195596
%50 = %RSP-8-16
%51 = load double, double* %50, align 1
%52 = bitcast double %51 to i64
%53 = zext double %51 to i128
%54 = and i128 %XMM0_1, -18446744073709551616
%XMM0_2 = or i128 %53, %54
%55 = zext i128 %XMM0_2 to i256
%56 = and i256 %YMM0_1, -340282366920938463463374607431768211456
%YMM0_2 = or i256 %55, %56
%57 = zext i128 %XMM0_2 to i512
%58 = and i512 %ZMM0_1, -340282366920938463463374607431768211456
%ZMM0_2 = or i512 %57, %58
%RIP_12 = 4195598
%EIP_11 = 4195598
%RAX_0 = %RAX
%EAX_0 = %RAX
%60 = and i32 %RAX, -256
%EAX_1 = or i32 %60, 1
%61 = and i64 %RAX, -256
%RAX_1 = or i64 %61, 1
%RIP_13 = 4195603
%EIP_12 = 4195603
%62 = %RSP-56-8
store i64 4195603, i64* %62
%ESP_2 = %RSP-64
store i32 %EAX_1, i32* %EAX
store i32 %EBP_0, i32* %EBP
store i32 4196000, i32* %EDI
%ZF_0 = icmp eq i64 %RSP_2, 0
%SF_0 = icmp slt i64 %RSP-56, 0
%63 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP-8, i64 48)
%OF_0 = extractvalue { i64, i1 } %63, 1
%64 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP-8, i64 48)
%CF_0 = extractvalue { i64, i1 } %64, 1
%65 = %RSP-56
%66 = call i8 @llvm.ctpop.i8(i8 %65)
%67 = trunc i8 %66 to i1
%PF_0 = icmp eq i8 %66, false
%68 = zext i1 %CF_0 to i32
%69 = shl i1 %CF_0, 0
%71 = zext i1 %PF_0 to i32
%72 = shl i1 %PF_0, 2
%74 = zext i1 false to i32
%75 = shl i1 false, 4
%77 = zext i1 %ZF_0 to i32
%78 = shl i1 %ZF_0, 6
%80 = zext i1 %SF_0 to i32
%81 = shl i1 %SF_0, 7
%83 = zext i1 %OF_0 to i32
%84 = shl i1 %OF_0, 11
store i32 %EIP_12, i32* %EIP
store i32 %ESI_0, i32* %ESI
store i32 %ESP_2, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RSP_1, i64* %RBP
store i64 4196000, i64* %RDI
store i64 %RIP_13, i64* %RIP
store i64 %RSI_1, i64* %RSI
store i64 %RSP_3, i64* %RSP
%85 = bitcast i128 %XMM0_2 to <4 x float>
store i128 %XMM0_2, <4 x float>* %XMM0
%86 = bitcast i256 %YMM0_2 to <8 x float>
store i256 %YMM0_2, <8 x float>* %YMM0
%87 = bitcast i512 %ZMM0_2 to <16 x float>
store i512 %ZMM0_2, <16 x float>* %ZMM0
%90 = load i64, i64* %RAX
store i64 %90, i64* %RAX_ptr
%91 = load i64, i64* %RBP
store i64 %91, i64* %RBP_ptr
%92 = load i64, i64* %RCX
store i64 %92, i64* %RCX_ptr
%93 = load i64, i64* %RDI
store i64 %93, i64* %RDI_ptr
%94 = load i64, i64* %RIP
store i64 %94, i64* %RIP_ptr
%95 = load i64, i64* %RSI
store i64 %95, i64* %RSI_ptr
%96 = load i64, i64* %RSP
store i64 %96, i64* %RSP_ptr
%97 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %97, <16 x float>* %ZMM0_ptr
%98 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %98, <16 x float>* %ZMM1_ptr
call void @fn_4003D0(%regset* %0)
%101 = load i64, i64* %RAX_ptr
store i64 %101, i64* %RAX
%102 = load i64, i64* %RBP_ptr
store i64 %102, i64* %RBP
%103 = load i64, i64* %RCX_ptr
store i64 %103, i64* %RCX
%104 = load i64, i64* %RDI_ptr
store i64 %104, i64* %RDI
%105 = load i64, i64* %RIP_ptr
store i64 %105, i64* %RIP
%106 = load i64, i64* %RSI_ptr
store i64 %106, i64* %RSI
%107 = load i64, i64* %RSP_ptr
store i64 %107, i64* %RSP
%108 = load <16 x float>, <16 x float>* %ZMM0_ptr
store <16 x float> %108, <16 x float>* %ZMM0
%109 = load <16 x float>, <16 x float>* %ZMM1_ptr
store <16 x float> %109, <16 x float>* %ZMM1
%RIP_14 = %RIP
%RIP_15 = %RIP+7
%EIP_13 = %RIP+7
%RBP_1 = %RBP
%111 = %RBP-24
store i32 0, i32* %111, align 1
%RIP_16 = %RIP+10
%EIP_14 = %RIP+10
%RAX_2 = %RAX
%EAX_2 = %RAX
%113 = %RBP-36
store i32 %EAX_2, i32* %113, align 1
store i32 %EAX_2, i32* %EAX
store i32 %EIP_14, i32* %EIP
store i64 %RAX_2, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 %RIP_16, i64* %RIP
br label %bb_40051D
bb_40051D:                                        ; preds = %bb_4005DD, %bb_4004D0
%RIP_85 = 4195616
%EIP_73 = 4195616
%RBP_10 = %RBP
%115 = %RBP-24
%EAX_18 = [ %RBP -24 ]
%RAX_22 = %RAX
%RAX_23 = %RBP-24
%116 = lshr i32 [ %RBP -24 ], 8
%RIP_86 = 4195619
%EIP_74 = 4195619
%118 = %RBP-8
%119 = load i32, i32* %118, align 1
%CC_A_0 = icmp ugt i32 %EAX_18, %119
%CC_AE_0 = icmp uge i32 %EAX_18, %119
%CC_B_0 = icmp ult i32 %EAX_18, %119
%CC_BE_014 = icmp ule i32 %EAX_18, %119
%CC_L_0 = icmp slt i32 %EAX_18, %119
%CC_LE_0 = icmp sle i32 %EAX_18, %119
%CC_G_0 = icmp sgt i32 %EAX_18, %119
%CC_GE_0 = icmp sge i32 %EAX_18, %119
%CC_E_0 = icmp eq i32 %EAX_18, %119
%CC_NE_0 = icmp ne i32 %EAX_18, %119
%120 = %119-[ %RBP -24 ]
%ZF_015 = icmp eq i32 %120, 0
%SF_016 = icmp slt i32 %120, 0
%121 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_18, i32 %119)
%OF_017 = extractvalue { i32, i1 } %121, 1
%122 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_18, i32 %119)
%CF_018 = extractvalue { i32, i1 } %122, 1
%123 = trunc i32 %120 to i8
%124 = call i32 @llvm.ctpop.i8(i8 %120)
%125 = trunc i8 %124 to i1
%PF_019 = icmp eq i8 %124, false
%126 = zext i1 %CF_018 to i32
%127 = shl i1 %CF_018, 0
%129 = zext i1 %PF_019 to i32
%130 = shl i1 %PF_019, 2
%132 = zext i1 false to i32
%133 = shl i1 false, 4
%135 = zext i1 %ZF_015 to i32
%136 = shl i1 %ZF_015, 6
%138 = zext i1 %SF_016 to i32
%139 = shl i1 %SF_016, 7
%141 = zext i1 %OF_017 to i32
%142 = shl i1 %OF_017, 11
%RIP_87 = 4195625
%EIP_75 = 4195625
store i32 %EAX_18, i32* %EAX
store i32 4195819, i32* %EIP
store i64 %RAX_23, i64* %RAX
store i64 %RBP_10, i64* %RBP
store i64 4195819, i64* %RIP
br i1 %CC_GE_0, label %bb_4005EB, label %bb_400529
bb_400529:                                        ; preds = %bb_40051D
%RIP_18 = 4195630
%EIP_15 = 4195630
%RBP_2 = %RBP
%144 = %RBP-16
%145 = load double, double* %144, align 1
%146 = bitcast double %145 to i64
%ZMM0_3 = %ZMM0
%147 = %ZMM0
%XMM0_3 = %ZMM0
%148 = zext double %145 to i128
%149 = and i128 %ZMM0, -18446744073709551616
%XMM0_4 = or i128 %148, %149
%150 = %ZMM0
%YMM0_3 = %ZMM0
%151 = zext i128 %XMM0_4 to i256
%152 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_4 = or i256 %151, %152
%153 = %ZMM0
%154 = zext i128 %XMM0_4 to i512
%155 = and i512 %153, -340282366920938463463374607431768211456
%ZMM0_4 = or i512 %154, %155
%RIP_19 = 4195633
%EIP_16 = 4195633
%157 = %RBP-8
%EAX_3 = [ %RBP -8 ]
%RAX_3 = %RAX
%RAX_4 = %RBP-8
%158 = lshr i32 [ %RBP -8 ], 8
%RIP_20 = 4195637
%EIP_17 = 4195637
%159 = [ %RBP -8 ]
%160 = bitcast double %159 to i64
%ZMM1_0 = %ZMM1
%161 = %ZMM1
%XMM1_0 = %ZMM1
%162 = zext double %159 to i128
%163 = and i128 %ZMM1, -18446744073709551616
%XMM1_1 = or i128 %162, %163
%164 = %ZMM1
%YMM1_0 = %ZMM1
%165 = zext i128 %XMM1_1 to i256
%166 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_1 = or i256 %165, %166
%167 = %ZMM1
%168 = zext i128 %XMM1_1 to i512
%169 = and i512 %167, -340282366920938463463374607431768211456
%ZMM1_1 = or i512 %168, %169
%RIP_21 = 4195641
%EIP_18 = 4195641
%170 = trunc i128 %XMM0_4 to i64
%171 = bitcast i128 %XMM0_4 to double
%172 = trunc i128 %XMM1_1 to i64
%173 = bitcast i128 %XMM1_1 to double
%174 = fdiv i128 %XMM0_4, %XMM1_1
%175 = bitcast double %174 to i64
%176 = zext double %174 to i128
%177 = and i128 %XMM0_4, -18446744073709551616
%XMM0_5 = or i128 %176, %177
%178 = zext i128 %XMM0_5 to i256
%179 = and i256 %YMM0_4, -340282366920938463463374607431768211456
%YMM0_5 = or i256 %178, %179
%180 = zext i128 %XMM0_5 to i512
%181 = and i512 %ZMM0_4, -340282366920938463463374607431768211456
%ZMM0_5 = or i512 %180, %181
%RIP_22 = 4195646
%EIP_19 = 4195646
%182 = trunc i128 %XMM0_5 to i64
%183 = bitcast i128 %XMM0_5 to double
%185 = %RBP-32
store i128 %XMM0_5, double* %185, align 1
%RIP_23 = 4195651
%EIP_20 = 4195651
%187 = %RBP-32
%188 = load double, double* %187, align 1
%189 = bitcast double %188 to i64
%190 = zext double %188 to i128
%191 = and i128 %XMM0_5, -18446744073709551616
%XMM0_6 = or i128 %190, %191
%192 = zext i128 %XMM0_6 to i256
%193 = and i256 %YMM0_5, -340282366920938463463374607431768211456
%YMM0_6 = or i256 %192, %193
%194 = zext i128 %XMM0_6 to i512
%195 = and i512 %ZMM0_5, -340282366920938463463374607431768211456
%ZMM0_6 = or i512 %194, %195
%RIP_24 = 4195654
%EIP_21 = 4195654
%197 = %RBP-20
%EAX_4 = [ %RBP -20 ]
%RAX_5 = %RBP-20
%198 = lshr i32 [ %RBP -20 ], 8
%RIP_25 = 4195658
%EIP_22 = 4195658
%199 = [ %RBP -20 ]
%200 = bitcast double %199 to i64
%201 = zext double %199 to i128
%202 = and i128 %XMM1_1, -18446744073709551616
%XMM1_2 = or i128 %201, %202
%203 = zext i128 %XMM1_2 to i256
%204 = and i256 %YMM1_1, -340282366920938463463374607431768211456
%YMM1_2 = or i256 %203, %204
%205 = zext i128 %XMM1_2 to i512
%206 = and i512 %ZMM1_1, -340282366920938463463374607431768211456
%ZMM1_2 = or i512 %205, %206
%RIP_26 = 4195662
%EIP_23 = 4195662
%207 = trunc i128 %XMM1_2 to i64
%208 = bitcast i128 %XMM1_2 to double
%209 = trunc i128 %XMM0_6 to i64
%210 = bitcast i128 %XMM0_6 to double
%ZF_01 = fcmp ueq double %208, %210
%PF_02 = fcmp uno i128 %XMM1_2, %XMM0_6
%CF_03 = fcmp ult i128 %XMM1_2, %XMM0_6
%211 = zext i1 %CF_03 to i32
%212 = shl i1 %CF_03, 0
%214 = zext i1 %PF_02 to i32
%215 = shl i1 %PF_02, 2
%217 = zext i1 false to i32
%218 = shl i1 false, 4
%220 = zext i1 %ZF_01 to i32
%221 = shl i1 %ZF_01, 6
%223 = zext i1 false to i32
%224 = shl i1 false, 7
%226 = zext i1 false to i32
%227 = shl i1 false, 11
%RIP_27 = 4195668
%EIP_24 = 4195668
%CC_BE_0 = or i1 %CF_03, %ZF_01
store i32 %EAX_4, i32* %EAX
store i32 4195721, i32* %EIP
store i64 %RAX_5, i64* %RAX
store i64 %RBP_2, i64* %RBP
store i64 4195721, i64* %RIP
%228 = bitcast i128 %XMM0_6 to <4 x float>
store i128 %XMM0_6, <4 x float>* %XMM0
%229 = bitcast i128 %XMM1_2 to <4 x float>
store i128 %XMM1_2, <4 x float>* %XMM1
%230 = bitcast i256 %YMM0_6 to <8 x float>
store i256 %YMM0_6, <8 x float>* %YMM0
%231 = bitcast i256 %YMM1_2 to <8 x float>
store i256 %YMM1_2, <8 x float>* %YMM1
%232 = bitcast i512 %ZMM0_6 to <16 x float>
store i512 %ZMM0_6, <16 x float>* %ZMM0
%233 = bitcast i512 %ZMM1_2 to <16 x float>
store i512 %ZMM1_2, <16 x float>* %ZMM1
br i1 %CC_BE_0, label %bb_400589, label %bb_400554
bb_400554:                                        ; preds = %bb_400529
%RIP_42 = 4195678
%EIP_36 = 4195678
%RIP_43 = 4195683
%EIP_37 = 4195683
%RBP_5 = %RBP
%235 = %RBP-32
%236 = load double, double* %235, align 1
%237 = bitcast double %236 to i64
%ZMM0_7 = %ZMM0
%238 = %ZMM0
%XMM0_7 = %ZMM0
%239 = zext double %236 to i128
%240 = and i128 %ZMM0, -18446744073709551616
%XMM0_8 = or i128 %239, %240
%241 = %ZMM0
%YMM0_7 = %ZMM0
%242 = zext i128 %XMM0_8 to i256
%243 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_8 = or i256 %242, %243
%244 = %ZMM0
%245 = zext i128 %XMM0_8 to i512
%246 = and i512 %244, -340282366920938463463374607431768211456
%ZMM0_8 = or i512 %245, %246
%RIP_44 = 4195688
%EIP_38 = 4195688
%247 = trunc i128 %XMM0_8 to i64
%248 = bitcast i128 %XMM0_8 to double
%250 = %RBP-16
%251 = load double, double* %250, align 1
%252 = fadd i128 %XMM0_8, %251
%253 = bitcast double %252 to i64
%254 = zext double %252 to i128
%255 = and i128 %XMM0_8, -18446744073709551616
%XMM0_9 = or i128 %254, %255
%256 = zext i128 %XMM0_9 to i256
%257 = and i256 %YMM0_8, -340282366920938463463374607431768211456
%YMM0_9 = or i256 %256, %257
%258 = zext i128 %XMM0_9 to i512
%259 = and i512 %ZMM0_8, -340282366920938463463374607431768211456
%ZMM0_9 = or i512 %258, %259
%RIP_45 = 4195693
%EIP_39 = 4195693
%260 = trunc i128 %XMM0_9 to i64
%261 = bitcast i128 %XMM0_9 to double
%263 = %RBP-16
store i128 %XMM0_9, double* %263, align 1
%RIP_46 = 4195698
%EIP_40 = 4195698
%265 = %RBP-32
%266 = load double, double* %265, align 1
%267 = bitcast double %266 to i64
%268 = zext double %266 to i128
%269 = and i128 %XMM0_9, -18446744073709551616
%XMM0_10 = or i128 %268, %269
%270 = zext i128 %XMM0_10 to i256
%271 = and i256 %YMM0_9, -340282366920938463463374607431768211456
%YMM0_10 = or i256 %270, %271
%272 = zext i128 %XMM0_10 to i512
%273 = and i512 %ZMM0_9, -340282366920938463463374607431768211456
%ZMM0_10 = or i512 %272, %273
%RIP_47 = 4195701
%EIP_41 = 4195701
%275 = %RBP-20
%ESI_1 = [ %RBP -20 ]
%RSI_2 = %RSI
%RSI_3 = %RBP-20
%RIP_48 = 4195706
%EIP_42 = 4195706
%277 = %RBP-16
%278 = load double, double* %277, align 1
%279 = bitcast double %278 to i64
%ZMM1_3 = %ZMM1
%280 = %ZMM1
%XMM1_3 = %ZMM1
%281 = zext double %278 to i128
%282 = and i128 %ZMM1, -18446744073709551616
%XMM1_4 = or i128 %281, %282
%283 = %ZMM1
%YMM1_3 = %ZMM1
%284 = zext i128 %XMM1_4 to i256
%285 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_4 = or i256 %284, %285
%286 = %ZMM1
%287 = zext i128 %XMM1_4 to i512
%288 = and i512 %286, -340282366920938463463374607431768211456
%ZMM1_4 = or i512 %287, %288
%RIP_49 = 4195708
%EIP_43 = 4195708
%RAX_10 = %RAX
%EAX_8 = %RAX
%290 = and i32 %RAX, -256
%EAX_9 = or i32 %290, 2
%291 = and i64 %RAX, -256
%RAX_11 = or i64 %291, 2
%RIP_50 = 4195713
%EIP_44 = 4195713
%RSP_10 = %RSP
%292 = %RSP-8
store i64 4195713, i64* %292
%ESP_7 = %RSP-8
store i32 %EAX_9, i32* %EAX
store i32 4196027, i32* %EDI
store i32 %EIP_44, i32* %EIP
store i32 %ESI_1, i32* %ESI
store i32 %ESP_7, i32* %ESP
store i64 %RAX_11, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 4196027, i64* %RDI
store i64 %RIP_50, i64* %RIP
store i64 %RSI_3, i64* %RSI
store i64 %RSP_11, i64* %RSP
%293 = bitcast i128 %XMM0_10 to <4 x float>
store i128 %XMM0_10, <4 x float>* %XMM0
%294 = bitcast i128 %XMM1_4 to <4 x float>
store i128 %XMM1_4, <4 x float>* %XMM1
%295 = bitcast i256 %YMM0_10 to <8 x float>
store i256 %YMM0_10, <8 x float>* %YMM0
%296 = bitcast i256 %YMM1_4 to <8 x float>
store i256 %YMM1_4, <8 x float>* %YMM1
%297 = bitcast i512 %ZMM0_10 to <16 x float>
store i512 %ZMM0_10, <16 x float>* %ZMM0
%298 = bitcast i512 %ZMM1_4 to <16 x float>
store i512 %ZMM1_4, <16 x float>* %ZMM1
%301 = load i64, i64* %RAX
store i64 %301, i64* %RAX_ptr
%302 = load i64, i64* %RBP
store i64 %302, i64* %RBP_ptr
%303 = load i64, i64* %RCX
store i64 %303, i64* %RCX_ptr
%304 = load i64, i64* %RDI
store i64 %304, i64* %RDI_ptr
%305 = load i64, i64* %RIP
store i64 %305, i64* %RIP_ptr
%306 = load i64, i64* %RSI
store i64 %306, i64* %RSI_ptr
%307 = load i64, i64* %RSP
store i64 %307, i64* %RSP_ptr
%308 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %308, <16 x float>* %ZMM0_ptr
%309 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %309, <16 x float>* %ZMM1_ptr
call void @fn_4003D0(%regset* %0)
%312 = load i64, i64* %RAX_ptr
store i64 %312, i64* %RAX
%313 = load i64, i64* %RBP_ptr
store i64 %313, i64* %RBP
%314 = load i64, i64* %RCX_ptr
store i64 %314, i64* %RCX
%315 = load i64, i64* %RDI_ptr
store i64 %315, i64* %RDI
%316 = load i64, i64* %RIP_ptr
store i64 %316, i64* %RIP
%317 = load i64, i64* %RSI_ptr
store i64 %317, i64* %RSI
%318 = load i64, i64* %RSP_ptr
store i64 %318, i64* %RSP
%319 = load <16 x float>, <16 x float>* %ZMM0_ptr
store <16 x float> %319, <16 x float>* %ZMM0
%320 = load <16 x float>, <16 x float>* %ZMM1_ptr
store <16 x float> %320, <16 x float>* %ZMM1
%RIP_51 = %RIP
%RIP_52 = %RIP+3
%EIP_45 = %RIP+3
%RAX_12 = %RAX
%EAX_10 = %RAX
%RBP_6 = %RBP
%322 = %RBP-40
store i32 %EAX_10, i32* %322, align 1
%RIP_53 = %RIP+8
%EIP_46 = %RIP+8
store i32 %EAX_10, i32* %EAX
store i32 4195800, i32* %EIP
store i64 %RAX_12, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195800, i64* %RIP
br label %bb_4005D8
bb_400589:                                        ; preds = %bb_400529
%RIP_56 = 4195731
%EIP_48 = 4195731
%RIP_57 = 4195736
%EIP_49 = 4195736
%RBP_7 = %RBP
%324 = %RBP-32
%325 = load double, double* %324, align 1
%326 = bitcast double %325 to i64
%ZMM0_11 = %ZMM0
%327 = %ZMM0
%XMM0_11 = %ZMM0
%328 = zext double %325 to i128
%329 = and i128 %ZMM0, -18446744073709551616
%XMM0_12 = or i128 %328, %329
%330 = %ZMM0
%YMM0_11 = %ZMM0
%331 = zext i128 %XMM0_12 to i256
%332 = and i256 %ZMM0, -340282366920938463463374607431768211456
%YMM0_12 = or i256 %331, %332
%333 = %ZMM0
%334 = zext i128 %XMM0_12 to i512
%335 = and i512 %333, -340282366920938463463374607431768211456
%ZMM0_12 = or i512 %334, %335
%RIP_58 = 4195739
%EIP_50 = 4195739
%337 = %RBP-20
%EAX_11 = [ %RBP -20 ]
%RAX_13 = %RAX
%RAX_14 = %RBP-20
%338 = lshr i32 [ %RBP -20 ], 8
%RIP_59 = 4195743
%EIP_51 = 4195743
%339 = [ %RBP -20 ]
%340 = bitcast double %339 to i64
%ZMM1_5 = %ZMM1
%341 = %ZMM1
%XMM1_5 = %ZMM1
%342 = zext double %339 to i128
%343 = and i128 %ZMM1, -18446744073709551616
%XMM1_6 = or i128 %342, %343
%344 = %ZMM1
%YMM1_5 = %ZMM1
%345 = zext i128 %XMM1_6 to i256
%346 = and i256 %ZMM1, -340282366920938463463374607431768211456
%YMM1_6 = or i256 %345, %346
%347 = %ZMM1
%348 = zext i128 %XMM1_6 to i512
%349 = and i512 %347, -340282366920938463463374607431768211456
%ZMM1_6 = or i512 %348, %349
%RIP_60 = 4195747
%EIP_52 = 4195747
%350 = trunc i128 %XMM1_6 to i64
%351 = bitcast i128 %XMM1_6 to double
%352 = trunc i128 %XMM0_12 to i64
%353 = bitcast i128 %XMM0_12 to double
%354 = fadd i128 %XMM1_6, %XMM0_12
%355 = bitcast double %354 to i64
%356 = zext double %354 to i128
%357 = and i128 %XMM1_6, -18446744073709551616
%XMM1_7 = or i128 %356, %357
%358 = zext i128 %XMM1_7 to i256
%359 = and i256 %YMM1_6, -340282366920938463463374607431768211456
%YMM1_7 = or i256 %358, %359
%360 = zext i128 %XMM1_7 to i512
%361 = and i512 %ZMM1_6, -340282366920938463463374607431768211456
%ZMM1_7 = or i512 %360, %361
%RIP_61 = 4195751
%EIP_53 = 4195751
%362 = trunc i128 %XMM1_7 to i64
%363 = bitcast i128 %XMM1_7 to double
%EAX_12 = %XMM1_7
%RAX_15 = %XMM1_7
%364 = lshr i32 %XMM1_7, 8
%RIP_62 = 4195754
%EIP_54 = 4195754
%366 = %RBP-20
store i32 %EAX_12, i32* %366, align 1
%RIP_63 = 4195759
%EIP_55 = 4195759
%368 = %RBP-32
%369 = load double, double* %368, align 1
%EAX_13 = %RBP-32
%RAX_16 = %RBP-32
%370 = lshr i32 %RBP-32, 8
%RIP_64 = 4195763
%EIP_56 = 4195763
%371 = %RBP-32
%372 = bitcast double %371 to i64
%373 = zext double %371 to i128
%374 = and i128 %XMM0_12, -18446744073709551616
%XMM0_13 = or i128 %373, %374
%375 = zext i128 %XMM0_13 to i256
%376 = and i256 %YMM0_12, -340282366920938463463374607431768211456
%YMM0_13 = or i256 %375, %376
%377 = zext i128 %XMM0_13 to i512
%378 = and i512 %ZMM0_12, -340282366920938463463374607431768211456
%ZMM0_13 = or i512 %377, %378
%RIP_65 = 4195768
%EIP_57 = 4195768
%380 = %RBP-16
%381 = load double, double* %380, align 1
%382 = bitcast double %381 to i64
%383 = zext double %381 to i128
%384 = and i128 %XMM1_7, -18446744073709551616
%XMM1_8 = or i128 %383, %384
%385 = zext i128 %XMM1_8 to i256
%386 = and i256 %YMM1_7, -340282366920938463463374607431768211456
%YMM1_8 = or i256 %385, %386
%387 = zext i128 %XMM1_8 to i512
%388 = and i512 %ZMM1_7, -340282366920938463463374607431768211456
%ZMM1_8 = or i512 %387, %388
%RIP_66 = 4195772
%EIP_58 = 4195772
%389 = trunc i128 %XMM1_8 to i64
%390 = bitcast i128 %XMM1_8 to double
%391 = trunc i128 %XMM0_13 to i64
%392 = bitcast i128 %XMM0_13 to double
%393 = fsub i128 %XMM1_8, %XMM0_13
%394 = bitcast double %393 to i64
%395 = zext double %393 to i128
%396 = and i128 %XMM1_8, -18446744073709551616
%XMM1_9 = or i128 %395, %396
%397 = zext i128 %XMM1_9 to i256
%398 = and i256 %YMM1_8, -340282366920938463463374607431768211456
%YMM1_9 = or i256 %397, %398
%399 = zext i128 %XMM1_9 to i512
%400 = and i512 %ZMM1_8, -340282366920938463463374607431768211456
%ZMM1_9 = or i512 %399, %400
%RIP_67 = 4195777
%EIP_59 = 4195777
%401 = trunc i128 %XMM1_9 to i64
%402 = bitcast i128 %XMM1_9 to double
%404 = %RBP-16
store i128 %XMM1_9, double* %404, align 1
%RIP_68 = 4195782
%EIP_60 = 4195782
%406 = %RBP-32
%407 = load double, double* %406, align 1
%408 = bitcast double %407 to i64
%409 = zext double %407 to i128
%410 = and i128 %XMM0_13, -18446744073709551616
%XMM0_14 = or i128 %409, %410
%411 = zext i128 %XMM0_14 to i256
%412 = and i256 %YMM0_13, -340282366920938463463374607431768211456
%YMM0_14 = or i256 %411, %412
%413 = zext i128 %XMM0_14 to i512
%414 = and i512 %ZMM0_13, -340282366920938463463374607431768211456
%ZMM0_14 = or i512 %413, %414
%RIP_69 = 4195785
%EIP_61 = 4195785
%416 = %RBP-20
%ESI_2 = [ %RBP -20 ]
%RSI_4 = %RSI
%RSI_5 = %RBP-20
%RIP_70 = 4195790
%EIP_62 = 4195790
%418 = %RBP-16
%419 = load double, double* %418, align 1
%420 = bitcast double %419 to i64
%421 = zext double %419 to i128
%422 = and i128 %XMM1_9, -18446744073709551616
%XMM1_10 = or i128 %421, %422
%423 = zext i128 %XMM1_10 to i256
%424 = and i256 %YMM1_9, -340282366920938463463374607431768211456
%YMM1_10 = or i256 %423, %424
%425 = zext i128 %XMM1_10 to i512
%426 = and i512 %ZMM1_9, -340282366920938463463374607431768211456
%ZMM1_10 = or i512 %425, %426
%RIP_71 = 4195792
%EIP_63 = 4195792
%428 = and i32 %RBP-32, -256
%EAX_14 = or i32 %428, 2
%429 = and i64 %RBP-32, -256
%RAX_17 = or i64 %429, 2
%RIP_72 = 4195797
%EIP_64 = 4195797
%RSP_12 = %RSP
%430 = %RSP-8
store i64 4195797, i64* %430
%ESP_8 = %RSP-8
store i32 %EAX_14, i32* %EAX
store i32 4196027, i32* %EDI
store i32 %EIP_64, i32* %EIP
store i32 %ESI_2, i32* %ESI
store i32 %ESP_8, i32* %ESP
store i64 %RAX_17, i64* %RAX
store i64 %RBP_7, i64* %RBP
store i64 4196027, i64* %RDI
store i64 %RIP_72, i64* %RIP
store i64 %RSI_5, i64* %RSI
store i64 %RSP_13, i64* %RSP
%431 = bitcast i128 %XMM0_14 to <4 x float>
store i128 %XMM0_14, <4 x float>* %XMM0
%432 = bitcast i128 %XMM1_10 to <4 x float>
store i128 %XMM1_10, <4 x float>* %XMM1
%433 = bitcast i256 %YMM0_14 to <8 x float>
store i256 %YMM0_14, <8 x float>* %YMM0
%434 = bitcast i256 %YMM1_10 to <8 x float>
store i256 %YMM1_10, <8 x float>* %YMM1
%435 = bitcast i512 %ZMM0_14 to <16 x float>
store i512 %ZMM0_14, <16 x float>* %ZMM0
%436 = bitcast i512 %ZMM1_10 to <16 x float>
store i512 %ZMM1_10, <16 x float>* %ZMM1
%439 = load i64, i64* %RAX
store i64 %439, i64* %RAX_ptr
%440 = load i64, i64* %RBP
store i64 %440, i64* %RBP_ptr
%441 = load i64, i64* %RCX
store i64 %441, i64* %RCX_ptr
%442 = load i64, i64* %RDI
store i64 %442, i64* %RDI_ptr
%443 = load i64, i64* %RIP
store i64 %443, i64* %RIP_ptr
%444 = load i64, i64* %RSI
store i64 %444, i64* %RSI_ptr
%445 = load i64, i64* %RSP
store i64 %445, i64* %RSP_ptr
%446 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %446, <16 x float>* %ZMM0_ptr
%447 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %447, <16 x float>* %ZMM1_ptr
call void @fn_4003D0(%regset* %0)
%450 = load i64, i64* %RAX_ptr
store i64 %450, i64* %RAX
%451 = load i64, i64* %RBP_ptr
store i64 %451, i64* %RBP
%452 = load i64, i64* %RCX_ptr
store i64 %452, i64* %RCX
%453 = load i64, i64* %RDI_ptr
store i64 %453, i64* %RDI
%454 = load i64, i64* %RIP_ptr
store i64 %454, i64* %RIP
%455 = load i64, i64* %RSI_ptr
store i64 %455, i64* %RSI
%456 = load i64, i64* %RSP_ptr
store i64 %456, i64* %RSP
%457 = load <16 x float>, <16 x float>* %ZMM0_ptr
store <16 x float> %457, <16 x float>* %ZMM0
%458 = load <16 x float>, <16 x float>* %ZMM1_ptr
store <16 x float> %458, <16 x float>* %ZMM1
%RIP_73 = %RIP
%RIP_74 = %RIP+3
%EIP_65 = %RIP+3
%RAX_18 = %RAX
%EAX_15 = %RAX
%RBP_8 = %RBP
%460 = %RBP-44
store i32 %EAX_15, i32* %460, align 1
store i32 %EAX_15, i32* %EAX
store i32 %EIP_65, i32* %EIP
store i64 %RAX_18, i64* %RAX
store i64 %RBP_8, i64* %RBP
store i64 %RIP_74, i64* %RIP
br label %bb_4005D8
bb_4005D8:                                        ; preds = %bb_400589, %bb_400554
%RIP_76 = 4195805
%EIP_66 = 4195805
store i32 4195805, i32* %EIP
store i64 4195805, i64* %RIP
br label %bb_4005DD
bb_4005DD:                                        ; preds = %bb_4005D8
%RIP_79 = 4195808
%EIP_68 = 4195808
%RBP_9 = %RBP
%462 = %RBP-24
%EAX_16 = [ %RBP -24 ]
%RAX_19 = %RAX
%RAX_20 = %RBP-24
%463 = lshr i32 [ %RBP -24 ], 8
%RIP_80 = 4195811
%EIP_69 = 4195811
%EAX_17 = %RBP-23
%RAX_21 = %RBP-23
%464 = lshr i32 %RBP-23, 8
%RIP_81 = 4195814
%EIP_70 = 4195814
%466 = %RBP-24
store i32 %EAX_17, i32* %466, align 1
%RIP_82 = 4195819
%EIP_71 = 4195819
%ZF_09 = icmp eq i32 %EAX_17, 0
%SF_010 = icmp slt i32 %RBP-23, 0
%467 = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 [ %RBP -24 ], i32 1)
%OF_011 = extractvalue { i32, i1 } %467, 1
%468 = call { i32, i1 } @llvm.uadd.with.overflow.i32(i32 [ %RBP -24 ], i32 1)
%CF_012 = extractvalue { i32, i1 } %468, 1
%469 = %RBP-23
%470 = call i8 @llvm.ctpop.i8(i8 %469)
%471 = trunc i8 %470 to i1
%PF_013 = icmp eq i8 %470, false
%472 = zext i1 %CF_012 to i32
%473 = shl i1 %CF_012, 0
%475 = zext i1 %PF_013 to i32
%476 = shl i1 %PF_013, 2
%478 = zext i1 false to i32
%479 = shl i1 false, 4
%481 = zext i1 %ZF_09 to i32
%482 = shl i1 %ZF_09, 6
%484 = zext i1 %SF_010 to i32
%485 = shl i1 %SF_010, 7
%487 = zext i1 %OF_011 to i32
%488 = shl i1 %OF_011, 11
store i32 %EAX_17, i32* %EAX
store i32 4195613, i32* %EIP
store i64 %RAX_21, i64* %RAX
store i64 %RBP_9, i64* %RBP
store i64 4195613, i64* %RIP
br label %bb_40051D
bb_4005EB:                                        ; preds = %bb_40051D
%RIP_30 = 4195829
%EIP_26 = 4195829
%RIP_31 = 4195831
%EIP_27 = 4195831
%RAX_6 = %RAX
%EAX_5 = %RAX
%490 = and i32 %RAX, -256
%EAX_6 = or i32 %490, 0
%491 = and i64 %RAX, -256
%RAX_7 = or i64 %491, 0
%RIP_32 = 4195836
%EIP_28 = 4195836
%RSP_4 = %RSP
%492 = %RSP-8
store i64 4195836, i64* %492
%ESP_3 = %RSP-8
store i32 %EAX_6, i32* %EAX
store i32 4196049, i32* %EDI
store i32 %EIP_28, i32* %EIP
store i32 %ESP_3, i32* %ESP
store i64 %RAX_7, i64* %RAX
store i64 4196049, i64* %RDI
store i64 %RIP_32, i64* %RIP
store i64 %RSP_5, i64* %RSP
%495 = load i64, i64* %RAX
store i64 %495, i64* %RAX_ptr
%496 = load i64, i64* %RBP
store i64 %496, i64* %RBP_ptr
%497 = load i64, i64* %RCX
store i64 %497, i64* %RCX_ptr
%498 = load i64, i64* %RDI
store i64 %498, i64* %RDI_ptr
%499 = load i64, i64* %RIP
store i64 %499, i64* %RIP_ptr
%500 = load i64, i64* %RSI
store i64 %500, i64* %RSI_ptr
%501 = load i64, i64* %RSP
store i64 %501, i64* %RSP_ptr
%502 = load <16 x float>, <16 x float>* %ZMM0
store <16 x float> %502, <16 x float>* %ZMM0_ptr
%503 = load <16 x float>, <16 x float>* %ZMM1
store <16 x float> %503, <16 x float>* %ZMM1_ptr
call void @fn_4003D0(%regset* %0)
%506 = load i64, i64* %RAX_ptr
store i64 %506, i64* %RAX
%507 = load i64, i64* %RBP_ptr
store i64 %507, i64* %RBP
%508 = load i64, i64* %RCX_ptr
store i64 %508, i64* %RCX
%509 = load i64, i64* %RDI_ptr
store i64 %509, i64* %RDI
%510 = load i64, i64* %RIP_ptr
store i64 %510, i64* %RIP
%511 = load i64, i64* %RSI_ptr
store i64 %511, i64* %RSI
%512 = load i64, i64* %RSP_ptr
store i64 %512, i64* %RSP
%513 = load <16 x float>, <16 x float>* %ZMM0_ptr
store <16 x float> %513, <16 x float>* %ZMM0
%514 = load <16 x float>, <16 x float>* %ZMM1_ptr
store <16 x float> %514, <16 x float>* %ZMM1
%RIP_33 = %RIP
%RIP_34 = %RIP+2
%EIP_29 = %RIP+2
%RCX_0 = %RCX
%ECX_0 = %RCX
%ECX_1 = xor i32 %ECX_0, %ECX_0
%RCX_1 = %ECX_1
%515 = lshr i32 %ECX_1, 8
%RIP_35 = %RIP+5
%EIP_30 = %RIP+5
%RAX_8 = %RAX
%EAX_7 = %RAX
%RBP_3 = %RBP
%517 = %RBP-48
store i32 %EAX_7, i32* %517, align 1
%RIP_36 = %RIP+7
%EIP_31 = %RIP+7
%RAX_9 = %ECX_1
%518 = lshr i32 %ECX_1, 8
%RIP_37 = %RIP+11
%EIP_32 = %RIP+11
%RSP_6 = %RSP
%RSP_7 = %RSP+48
%ESP_4 = %RSP+48
%RIP_38 = %RIP+12
%EIP_33 = %RIP+12
%RSP_8 = %RSP+56
%ESP_5 = %RSP+56
%520 = %RSP+56-8
%RBP_4 = [ %RSP +64 ]
%EBP_1 = %RSP+64
%RIP_39 = %RIP+13
%EIP_34 = %RIP+13
%521 = %RSP+56+8
%RIP_40 = [ %RSP +56 ]
%ESP_6 = %RSP+64
%EIP_35 = %RSP+56
%ZF_04 = icmp eq i64 %RSP_7, 0
%SF_05 = icmp slt i64 %RSP+48, 0
%522 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP, i64 48)
%OF_06 = extractvalue { i64, i1 } %522, 1
%523 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP, i64 48)
%CF_07 = extractvalue { i64, i1 } %523, 1
%524 = %RSP+48
%525 = call i8 @llvm.ctpop.i8(i8 %524)
%526 = trunc i8 %525 to i1
%PF_08 = icmp eq i8 %525, false
%527 = zext i1 %CF_07 to i32
%528 = shl i1 %CF_07, 0
%530 = zext i1 %PF_08 to i32
%531 = shl i1 %PF_08, 2
%533 = zext i1 false to i32
%534 = shl i1 false, 4
%536 = zext i1 %ZF_04 to i32
%537 = shl i1 %ZF_04, 6
%539 = zext i1 %SF_05 to i32
%540 = shl i1 %SF_05, 7
%542 = zext i1 %OF_06 to i32
%543 = shl i1 %OF_06, 11
store i32 %ECX_1, i32* %EAX
store i32 %EBP_1, i32* %EBP
store i32 %ECX_1, i32* %ECX
store i32 %EIP_35, i32* %EIP
store i32 %ESP_6, i32* %ESP
store i64 %RAX_9, i64* %RAX
store i64 %RBP_4, i64* %RBP
store i64 %RCX_1, i64* %RCX
store i64 %RIP_40, i64* %RIP
store i64 %RSP_9, i64* %RSP
br label %exit_fn_4004D0
}

