define void @fn_4003D0(%regset* noalias nocapture) {
entry_fn_4003D0:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = %RIP_ptr
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
br label %bb_4003D0
exit_fn_4003D0:                                   ; preds = %bb_4003D0
%1 = load i64, i64* %RIP
store i64 %1, i64* %RIP_ptr
ret void
bb_4003D0:                                        ; preds = %entry_fn_4003D0
%RIP_1 = 4195286
%EIP_0 = 4195286
%3 = 6295576
%RIP_2 = 6295576
%EIP_1 = 6295576
%4 = 6295576
%5 = call i8* @llvm.dc.translate.at(i8* 6295576)
%6 = %5
store i32 6295576, i32* %EIP
store i64 6295576, i64* %RIP
%7 = load i64, i64* %RIP
store i64 %7, i64* %RIP_ptr
call void %5(%regset* %0)
%8 = load i64, i64* %RIP_ptr
store i64 %8, i64* %RIP
br label %exit_fn_4003D0
}

define void @fn_4004D0(%regset* noalias nocapture) {
%ae = alloca i32, align 4
%ad = alloca i64, align 8
%ac = alloca i32, align 4
%ab = alloca i64, align 8
%k = alloca i32, align 4
%j = alloca i64, align 8
%i = alloca double, align 8
%h = alloca i32, align 4
%g = alloca i32, align 4
%f = alloca i64, align 8
%e = alloca i32, align 4
%d = alloca double, align 8
%c = alloca i32, align 4
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_4004D0:
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
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = %RDI_ptr
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = %RDI_ptr
%EDI = alloca i32
store i32 %EDI_init, i32* %EDI
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
%RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
%RSI_init = %RSI_ptr
%RSI = alloca i64
store i64 %RSI_init, i64* %RSI
%ESI_init = %RSI_ptr
%ESI = alloca i32
store i32 %ESI_init, i32* %ESI
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX_init = %RAX_ptr
%EAX = alloca i32
store i32 %EAX_init, i32* %EAX
%5 = lshr i64 %RAX_init, 8
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%6 = %ZMM1_init
%7 = %ZMM1_init
%XMM1_init = %ZMM1_ptr
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = %ZMM1_init
%9 = %ZMM1_init
%YMM1_init = %ZMM1_ptr
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = %RCX_ptr
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %RCX_init, 8
br label %bb_4004D0
exit_fn_4004D0: ; preds = %bb_4005EB
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
bb_4004D0: ; preds = %entry_fn_4004D0
%RIP_1 = 4195537
%EIP_0 = 4195537
%RBP_0 = %RBP
%RSP_0 = %RSP
%23 = %a
store i64 %RBP, i64* %a, align 1
%RSP_1 = %a
%ESP_0 = %a
%RIP_2 = 4195540
%EIP_1 = 4195540
%EBP_0 = %a
%RIP_3 = 4195544
%EIP_2 = 4195544
%RSP_2 = %ae
%ESP_1 = %ae
%RIP_4 = 4195554
%EIP_3 = 4195554
%RIP_5 = 4195562
%EIP_4 = 4195562
%25 = 4195992
%26 = load double, double* 4195992, align 1
%27 = %26
%ZMM0_0 = %ZMM0
%28 = %ZMM0
%XMM0_0 = %ZMM0
%XMM0_1 = %26
%31 = %ZMM0
%YMM0_0 = %ZMM0
%YMM0_1 = %26
%34 = %ZMM0
%ZMM0_1 = %26
%RIP_6 = 4195569
%EIP_5 = 4195569
%38 = %b
store i32 0, i32* %b, align 1
%RIP_7 = 4195576
%EIP_6 = 4195576
%40 = %c
store i32 5, i32* %c, align 1
%RIP_8 = 4195581
%EIP_7 = 4195581
%41 = %26
%42 = %26
%44 = %d
store double %26, double* %d, align 1
%RIP_9 = 4195588
%EIP_8 = 4195588
%46 = %e
store i32 100, i32* %e, align 1
%RIP_10 = 4195591
%EIP_9 = 4195591
%48 = %e
%ESI_0 = %e
%RSI_0 = %RSI
%RSI_1 = %e
%RIP_11 = 4195596
%EIP_10 = 4195596
%50 = %d
%51 = load double, double* %d, align 1
%52 = %51
%XMM0_2 = %51
%YMM0_2 = %51
%ZMM0_2 = %51
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
%RSP_3 = %f
%62 = %f
store i64 4195603, i64* %f
%ESP_2 = %f
store i32 %EAX_1, i32* %EAX
store i32 %a, i32* %EBP
store i32 4196000, i32* %EDI
%ZF_0 = icmp eq i64 %ae, 0
%SF_0 = icmp slt i64 %ae, 0
%63 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %a, i64 48)
%OF_0 = extractvalue { i64, i1 } %63, 1
%64 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %a, i64 48)
%CF_0 = extractvalue { i64, i1 } %64, 1
%65 = %ae
%66 = call i8 @llvm.ctpop.i8(i8 %ae)
%67 = %66
%PF_0 = icmp eq i1 %66, false
%68 = %64
%69 = shl i32 %64, 0
%71 = %PF_0
%72 = shl i32 %PF_0, 2
%74 = false
%75 = shl i32 false, 4
%77 = %ZF_0
%78 = shl i32 %ZF_0, 6
%80 = %SF_0
%81 = shl i32 %SF_0, 7
%83 = %63
%84 = shl i32 %63, 11
store i32 4195603, i32* %EIP
store i32 %e, i32* %ESI
store i32 %f, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %a, i64* %RBP
store i64 4196000, i64* %RDI
store i64 4195603, i64* %RIP
store i64 %e, i64* %RSI
store i64 %f, i64* %RSP
%85 = %51
store <4 x float> %51, <4 x float>* %XMM0
%86 = %51
store <8 x float> %51, <8 x float>* %YMM0
%87 = %51
store <16 x float> %51, <16 x float>* %ZMM0
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
%RIP_14 = 4195603
%RIP_15 = 4195610
%EIP_13 = 4195610
%RBP_1 = %a
%111 = %g
store i32 0, i32* %g, align 1
%RIP_16 = 4195613
%EIP_14 = 4195613
%RAX_2 = %RAX_1
%EAX_2 = %RAX_1
%113 = %h
store i32 %RAX_1, i32* %h, align 1
store i32 %RAX_1, i32* %EAX
store i32 4195613, i32* %EIP
store i64 %RAX_1, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195613, i64* %RIP
br label %bb_40051D
bb_40051D: ; preds = %bb_4005DD, %bb_4004D0
%RIP_85 = 4195616
%EIP_73 = 4195616
%RBP_10 = %a
%115 = %g
%EAX_18 = %g
%RAX_22 = %RAX_1
%RAX_23 = %g
%116 = lshr i32 %g, 8
%RIP_86 = 4195619
%EIP_74 = 4195619
%118 = %c
%119 = load i32, i32* %c, align 1
%CC_A_0 = icmp ugt i32 %g, %119
%CC_AE_0 = icmp uge i32 %g, %119
%CC_B_0 = icmp ult i32 %g, %119
%CC_BE_014 = icmp ule i32 %g, %119
%CC_L_0 = icmp slt i32 %g, %119
%CC_LE_0 = icmp sle i32 %g, %119
%CC_G_0 = icmp sgt i32 %g, %119
%CC_GE_0 = icmp sge i32 %g, %119
%CC_E_0 = icmp eq i32 %g, %119
%CC_NE_0 = icmp ne i32 %g, %119
%120 = %119-%g
%ZF_015 = icmp eq i32 %119-%g, 0
%SF_016 = icmp slt i32 %119-%g, 0
%121 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %g, i32 %119)
%OF_017 = extractvalue { i32, i1 } %121, 1
%122 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %g, i32 %119)
%CF_018 = extractvalue { i32, i1 } %122, 1
%123 = %119-%g
%124 = call i8 @llvm.ctpop.i8(i8 %119-%g)
%125 = %124
%PF_019 = icmp eq i1 %124, false
%126 = %122
%127 = shl i32 %122, 0
%129 = %PF_019
%130 = shl i32 %PF_019, 2
%132 = false
%133 = shl i32 false, 4
%135 = %ZF_015
%136 = shl i32 %ZF_015, 6
%138 = %SF_016
%139 = shl i32 %SF_016, 7
%141 = %121
%142 = shl i32 %121, 11
%RIP_87 = 4195625
%EIP_75 = 4195625
store i32 %g, i32* %EAX
store i32 4195819, i32* %EIP
store i64 %g, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195819, i64* %RIP
br i1 %CC_GE_0, label %bb_4005EB, label %bb_400529
bb_400529: ; preds = %bb_40051D
%RIP_18 = 4195630
%EIP_15 = 4195630
%RBP_2 = %a
%144 = %d
%145 = load double, double* %d, align 1
%146 = %145
%ZMM0_3 = %ZMM0_2
%147 = %ZMM0_2
%XMM0_3 = %ZMM0_2
%XMM0_4 = %145
%150 = %ZMM0_2
%YMM0_3 = %ZMM0_2
%YMM0_4 = %145
%153 = %ZMM0_2
%ZMM0_4 = %145
%RIP_19 = 4195633
%EIP_16 = 4195633
%157 = %c
%EAX_3 = %c
%RAX_3 = %g
%RAX_4 = %c
%158 = lshr i32 %c, 8
%RIP_20 = 4195637
%EIP_17 = 4195637
%159 = %c
%160 = %c
%ZMM1_0 = %ZMM1
%161 = %ZMM1
%XMM1_0 = %ZMM1
%XMM1_1 = %c
%164 = %ZMM1
%YMM1_0 = %ZMM1
%YMM1_1 = %c
%167 = %ZMM1
%ZMM1_1 = %c
%RIP_21 = 4195641
%EIP_18 = 4195641
%170 = %145
%171 = %145
%172 = %c
%173 = %c
%174 = fdiv double %145, %c
%175 = %174
%XMM0_5 = %174
%YMM0_5 = %174
%ZMM0_5 = %174
%RIP_22 = 4195646
%EIP_19 = 4195646
%182 = %174
%183 = %174
%185 = %i
store double %174, double* %i, align 1
%RIP_23 = 4195651
%EIP_20 = 4195651
%187 = %i
%188 = load double, double* %i, align 1
%189 = %188
%XMM0_6 = %188
%YMM0_6 = %188
%ZMM0_6 = %188
%RIP_24 = 4195654
%EIP_21 = 4195654
%197 = %e
%EAX_4 = %e
%RAX_5 = %e
%198 = lshr i32 %e, 8
%RIP_25 = 4195658
%EIP_22 = 4195658
%199 = %e
%200 = %e
%XMM1_2 = %e
%YMM1_2 = %e
%ZMM1_2 = %e
%RIP_26 = 4195662
%EIP_23 = 4195662
%207 = %e
%208 = %e
%209 = %188
%210 = %188
%ZF_01 = fcmp ueq double %e, %188
%PF_02 = fcmp uno double %e, %188
%CF_03 = fcmp ult double %e, %188
%211 = %CF_03
%212 = shl i32 %CF_03, 0
%214 = %PF_02
%215 = shl i32 %PF_02, 2
%217 = false
%218 = shl i32 false, 4
%220 = %ZF_01
%221 = shl i32 %ZF_01, 6
%223 = false
%224 = shl i32 false, 7
%226 = false
%227 = shl i32 false, 11
%RIP_27 = 4195668
%EIP_24 = 4195668
%CC_BE_0 = or i1 %CF_03, %ZF_01
store i32 %e, i32* %EAX
store i32 4195721, i32* %EIP
store i64 %e, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195721, i64* %RIP
%228 = %188
store <4 x float> %188, <4 x float>* %XMM0
%229 = %e
store <4 x float> %e, <4 x float>* %XMM1
%230 = %188
store <8 x float> %188, <8 x float>* %YMM0
%231 = %e
store <8 x float> %e, <8 x float>* %YMM1
%232 = %188
store <16 x float> %188, <16 x float>* %ZMM0
%233 = %e
store <16 x float> %e, <16 x float>* %ZMM1
br i1 %CC_BE_0, label %bb_400589, label %bb_400554
bb_400554: ; preds = %bb_400529
%RIP_42 = 4195678
%EIP_36 = 4195678
%RIP_43 = 4195683
%EIP_37 = 4195683
%RBP_5 = %a
%235 = %i
%236 = load double, double* %i, align 1
%237 = %236
%ZMM0_7 = %ZMM0_6
%238 = %ZMM0_6
%XMM0_7 = %ZMM0_6
%XMM0_8 = %236
%241 = %ZMM0_6
%YMM0_7 = %ZMM0_6
%YMM0_8 = %236
%244 = %ZMM0_6
%ZMM0_8 = %236
%RIP_44 = 4195688
%EIP_38 = 4195688
%247 = %236
%248 = %236
%250 = %d
%251 = load double, double* %d, align 1
%252 = fadd double %236, %251
%253 = %252
%XMM0_9 = %252
%YMM0_9 = %252
%ZMM0_9 = %252
%RIP_45 = 4195693
%EIP_39 = 4195693
%260 = %252
%261 = %252
%263 = %d
store double %252, double* %d, align 1
%RIP_46 = 4195698
%EIP_40 = 4195698
%265 = %i
%266 = load double, double* %i, align 1
%267 = %266
%XMM0_10 = %266
%YMM0_10 = %266
%ZMM0_10 = %266
%RIP_47 = 4195701
%EIP_41 = 4195701
%275 = %e
%ESI_1 = %e
%RSI_2 = %e
%RSI_3 = %e
%RIP_48 = 4195706
%EIP_42 = 4195706
%277 = %d
%278 = load double, double* %d, align 1
%279 = %278
%ZMM1_3 = %ZMM1_2
%280 = %ZMM1_2
%XMM1_3 = %ZMM1_2
%XMM1_4 = %278
%283 = %ZMM1_2
%YMM1_3 = %ZMM1_2
%YMM1_4 = %278
%286 = %ZMM1_2
%ZMM1_4 = %278
%RIP_49 = 4195708
%EIP_43 = 4195708
%RAX_10 = %e
%EAX_8 = %e
%290 = and i32 %e, -256
%EAX_9 = or i32 %290, 2
%291 = and i64 %e, -256
%RAX_11 = or i64 %291, 2
%RIP_50 = 4195713
%EIP_44 = 4195713
%RSP_10 = %f
%RSP_11 = %j
%292 = %j
store i64 4195713, i64* %j
%ESP_7 = %j
store i32 %EAX_9, i32* %EAX
store i32 4196027, i32* %EDI
store i32 4195713, i32* %EIP
store i32 %e, i32* %ESI
store i32 %j, i32* %ESP
store i64 %RAX_11, i64* %RAX
store i64 %a, i64* %RBP
store i64 4196027, i64* %RDI
store i64 4195713, i64* %RIP
store i64 %e, i64* %RSI
store i64 %j, i64* %RSP
%293 = %266
store <4 x float> %266, <4 x float>* %XMM0
%294 = %278
store <4 x float> %278, <4 x float>* %XMM1
%295 = %266
store <8 x float> %266, <8 x float>* %YMM0
%296 = %278
store <8 x float> %278, <8 x float>* %YMM1
%297 = %266
store <16 x float> %266, <16 x float>* %ZMM0
%298 = %278
store <16 x float> %278, <16 x float>* %ZMM1
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
%RIP_51 = 4195713
%RIP_52 = 4195716
%EIP_45 = 4195716
%RAX_12 = %RAX_11
%EAX_10 = %RAX_11
%RBP_6 = %a
%322 = %k
store i32 %RAX_11, i32* %k, align 1
%RIP_53 = 4195721
%EIP_46 = 4195721
store i32 %RAX_11, i32* %EAX
store i32 4195800, i32* %EIP
store i64 %RAX_11, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195800, i64* %RIP
br label %bb_4005D8
bb_400589: ; preds = %bb_400529
%RIP_56 = 4195731
%EIP_48 = 4195731
%RIP_57 = 4195736
%EIP_49 = 4195736
%RBP_7 = %a
%324 = %i
%325 = load double, double* %i, align 1
%326 = %325
%ZMM0_11 = %ZMM0_10
%327 = %ZMM0_10
%XMM0_11 = %ZMM0_10
%XMM0_12 = %325
%330 = %ZMM0_10
%YMM0_11 = %ZMM0_10
%YMM0_12 = %325
%333 = %ZMM0_10
%ZMM0_12 = %325
%RIP_58 = 4195739
%EIP_50 = 4195739
%337 = %e
%EAX_11 = %e
%RAX_13 = %RAX_11
%RAX_14 = %e
%338 = lshr i32 %e, 8
%RIP_59 = 4195743
%EIP_51 = 4195743
%339 = %e
%340 = %e
%ZMM1_5 = %ZMM1_4
%341 = %ZMM1_4
%XMM1_5 = %ZMM1_4
%XMM1_6 = %e
%344 = %ZMM1_4
%YMM1_5 = %ZMM1_4
%YMM1_6 = %e
%347 = %ZMM1_4
%ZMM1_6 = %e
%RIP_60 = 4195747
%EIP_52 = 4195747
%350 = %e
%351 = %e
%352 = %325
%353 = %325
%354 = fadd double %e, %325
%355 = %354
%XMM1_7 = %354
%YMM1_7 = %354
%ZMM1_7 = %354
%RIP_61 = 4195751
%EIP_53 = 4195751
%362 = %354
%363 = %354
%EAX_12 = %XMM1_7
%RAX_15 = %XMM1_7
%364 = lshr i32 %XMM1_7, 8
%RIP_62 = 4195754
%EIP_54 = 4195754
%366 = %e
store i32 %XMM1_7, i32* %e, align 1
%RIP_63 = 4195759
%EIP_55 = 4195759
%368 = %i
%369 = load double, double* %i, align 1
%EAX_13 = %i
%RAX_16 = %i
%370 = lshr i32 %i, 8
%RIP_64 = 4195763
%EIP_56 = 4195763
%371 = %i
%372 = %i
%XMM0_13 = %i
%YMM0_13 = %i
%ZMM0_13 = %i
%RIP_65 = 4195768
%EIP_57 = 4195768
%380 = %d
%381 = load double, double* %d, align 1
%382 = %381
%XMM1_8 = %381
%YMM1_8 = %381
%ZMM1_8 = %381
%RIP_66 = 4195772
%EIP_58 = 4195772
%389 = %381
%390 = %381
%391 = %i
%392 = %i
%393 = fsub double %381, %i
%394 = %393
%XMM1_9 = %393
%YMM1_9 = %393
%ZMM1_9 = %393
%RIP_67 = 4195777
%EIP_59 = 4195777
%401 = %393
%402 = %393
%404 = %d
store double %393, double* %d, align 1
%RIP_68 = 4195782
%EIP_60 = 4195782
%406 = %i
%407 = load double, double* %i, align 1
%408 = %407
%XMM0_14 = %407
%YMM0_14 = %407
%ZMM0_14 = %407
%RIP_69 = 4195785
%EIP_61 = 4195785
%416 = %e
%ESI_2 = %e
%RSI_4 = %e
%RSI_5 = %e
%RIP_70 = 4195790
%EIP_62 = 4195790
%418 = %d
%419 = load double, double* %d, align 1
%420 = %419
%XMM1_10 = %419
%YMM1_10 = %419
%ZMM1_10 = %419
%RIP_71 = 4195792
%EIP_63 = 4195792
%428 = and i32 %i, -256
%EAX_14 = or i32 %428, 2
%429 = and i64 %i, -256
%RAX_17 = or i64 %429, 2
%RIP_72 = 4195797
%EIP_64 = 4195797
%RSP_12 = %j
%RSP_13 = %ab
%430 = %ab
store i64 4195797, i64* %ab
%ESP_8 = %ab
store i32 %EAX_14, i32* %EAX
store i32 4196027, i32* %EDI
store i32 4195797, i32* %EIP
store i32 %e, i32* %ESI
store i32 %ab, i32* %ESP
store i64 %RAX_17, i64* %RAX
store i64 %a, i64* %RBP
store i64 4196027, i64* %RDI
store i64 4195797, i64* %RIP
store i64 %e, i64* %RSI
store i64 %ab, i64* %RSP
%431 = %407
store <4 x float> %407, <4 x float>* %XMM0
%432 = %419
store <4 x float> %419, <4 x float>* %XMM1
%433 = %407
store <8 x float> %407, <8 x float>* %YMM0
%434 = %419
store <8 x float> %419, <8 x float>* %YMM1
%435 = %407
store <16 x float> %407, <16 x float>* %ZMM0
%436 = %419
store <16 x float> %419, <16 x float>* %ZMM1
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
%RIP_73 = 4195797
%RIP_74 = 4195800
%EIP_65 = 4195800
%RAX_18 = %RAX_17
%EAX_15 = %RAX_17
%RBP_8 = %a
%460 = %ac
store i32 %RAX_17, i32* %ac, align 1
store i32 %RAX_17, i32* %EAX
store i32 4195800, i32* %EIP
store i64 %RAX_17, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195800, i64* %RIP
br label %bb_4005D8
bb_4005D8: ; preds = %bb_400589, %bb_400554
%RIP_76 = 4195805
%EIP_66 = 4195805
store i32 4195805, i32* %EIP
store i64 4195805, i64* %RIP
br label %bb_4005DD
bb_4005DD: ; preds = %bb_4005D8
%RIP_79 = 4195808
%EIP_68 = 4195808
%RBP_9 = %a
%462 = %g
%EAX_16 = %g
%RAX_19 = %RAX_17
%RAX_20 = %g
%463 = lshr i32 %g, 8
%RIP_80 = 4195811
%EIP_69 = 4195811
%EAX_17 = %RSP-31
%RAX_21 = %RSP-31
%464 = lshr i32 %RSP-31, 8
%RIP_81 = 4195814
%EIP_70 = 4195814
%466 = %g
store i32 %RSP-31, i32* %g, align 1
%RIP_82 = 4195819
%EIP_71 = 4195819
%ZF_09 = icmp eq i32 %RSP-31, 0
%SF_010 = icmp slt i32 %RSP-31, 0
%467 = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %g, i32 1)
%OF_011 = extractvalue { i32, i1 } %467, 1
%468 = call { i32, i1 } @llvm.uadd.with.overflow.i32(i32 %g, i32 1)
%CF_012 = extractvalue { i32, i1 } %468, 1
%469 = %RSP-31
%470 = call i8 @llvm.ctpop.i8(i8 %RSP-31)
%471 = %470
%PF_013 = icmp eq i1 %470, false
%472 = %468
%473 = shl i32 %468, 0
%475 = %PF_013
%476 = shl i32 %PF_013, 2
%478 = false
%479 = shl i32 false, 4
%481 = %ZF_09
%482 = shl i32 %ZF_09, 6
%484 = %SF_010
%485 = shl i32 %SF_010, 7
%487 = %467
%488 = shl i32 %467, 11
store i32 %RSP-31, i32* %EAX
store i32 4195613, i32* %EIP
store i64 %RSP-31, i64* %RAX
store i64 %a, i64* %RBP
store i64 4195613, i64* %RIP
br label %bb_40051D
bb_4005EB: ; preds = %bb_40051D
%RIP_30 = 4195829
%EIP_26 = 4195829
%RIP_31 = 4195831
%EIP_27 = 4195831
%RAX_6 = %RSP-31
%EAX_5 = %RSP-31
%490 = and i32 %RSP-31, -256
%EAX_6 = or i32 %490, 0
%491 = and i64 %RSP-31, -256
%RAX_7 = or i64 %491, 0
%RIP_32 = 4195836
%EIP_28 = 4195836
%RSP_4 = %ab
%RSP_5 = %ad
%492 = %ad
store i64 4195836, i64* %ad
%ESP_3 = %ad
store i32 %EAX_6, i32* %EAX
store i32 4196049, i32* %EDI
store i32 4195836, i32* %EIP
store i32 %ad, i32* %ESP
store i64 %RAX_7, i64* %RAX
store i64 4196049, i64* %RDI
store i64 4195836, i64* %RIP
store i64 %ad, i64* %RSP
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
%RIP_33 = 4195836
%RIP_34 = 4195838
%EIP_29 = 4195838
%RCX_0 = %RCX
%ECX_0 = %RCX
%ECX_1 = xor i32 %ECX_0, %ECX_0
%RCX_1 = %ECX_1
%515 = lshr i32 %ECX_1, 8
%RIP_35 = 4195841
%EIP_30 = 4195841
%RAX_8 = %RAX_7
%EAX_7 = %RAX_7
%RBP_3 = %a
%517 = %ae
store i32 %RAX_7, i32* %ae, align 1
%RIP_36 = 4195843
%EIP_31 = 4195843
%RAX_9 = %ECX_1
%518 = lshr i32 %ECX_1, 8
%RIP_37 = 4195847
%EIP_32 = 4195847
%RSP_6 = %ad
%RSP_7 = %i
%ESP_4 = %i
%RIP_38 = 4195848
%EIP_33 = 4195848
%RSP_8 = %g
%ESP_5 = %g
%520 = %i
%RBP_4 = %i
%EBP_1 = %i
%RIP_39 = 4195849
%EIP_34 = 4195849
%RSP_9 = %d
%521 = %g
%RIP_40 = %g
%ESP_6 = %d
%EIP_35 = %g
%ZF_04 = icmp eq i64 %i, 0
%SF_05 = icmp slt i64 %i, 0
%522 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %ad, i64 48)
%OF_06 = extractvalue { i64, i1 } %522, 1
%523 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %ad, i64 48)
%CF_07 = extractvalue { i64, i1 } %523, 1
%524 = %i
%525 = call i8 @llvm.ctpop.i8(i8 %i)
%526 = %525
%PF_08 = icmp eq i1 %525, false
%527 = %523
%528 = shl i32 %523, 0
%530 = %PF_08
%531 = shl i32 %PF_08, 2
%533 = false
%534 = shl i32 false, 4
%536 = %ZF_04
%537 = shl i32 %ZF_04, 6
%539 = %SF_05
%540 = shl i32 %SF_05, 7
%542 = %522
%543 = shl i32 %522, 11
store i32 %ECX_1, i32* %EAX
store i32 %i, i32* %EBP
store i32 %ECX_1, i32* %ECX
store i32 %g, i32* %EIP
store i32 %d, i32* %ESP
store i64 %ECX_1, i64* %RAX
store i64 %i, i64* %RBP
store i64 %ECX_1, i64* %RCX
store i64 %g, i64* %RIP
store i64 %d, i64* %RSP
br label %exit_fn_4004D0
}

