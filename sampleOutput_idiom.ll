define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
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
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = load i64, i64* %RDI_ptr
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI_init = trunc i64 %RDI_init to i32
%EDI = alloca i32
store i32 %EDI_init, i32* %EDI
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%1 = bitcast <16 x float> %ZMM0_init to i512
%2 = trunc i512 %1 to i128
%XMM0_init = bitcast i128 %2 to <4 x float>
%XMM0 = alloca <4 x float>
store <4 x float> %XMM0_init, <4 x float>* %XMM0
%3 = bitcast <16 x float> %ZMM0_init to i512
%4 = trunc i512 %3 to i256
%YMM0_init = bitcast i256 %4 to <8 x float>
%YMM0 = alloca <8 x float>
store <8 x float> %YMM0_init, <8 x float>* %YMM0
%RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
%RSI_init = load i64, i64* %RSI_ptr
%RSI = alloca i64
store i64 %RSI_init, i64* %RSI
%ESI_init = trunc i64 %RSI_init to i32
%ESI = alloca i32
store i32 %ESI_init, i32* %ESI
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
%6 = bitcast <16 x float> %ZMM1_init to i512
%7 = trunc i512 %6 to i128
%XMM1_init = bitcast i128 %7 to <4 x float>
%XMM1 = alloca <4 x float>
store <4 x float> %XMM1_init, <4 x float>* %XMM1
%8 = bitcast <16 x float> %ZMM1_init to i512
%9 = trunc i512 %8 to i256
%YMM1_init = bitcast i256 %9 to <8 x float>
%YMM1 = alloca <8 x float>
store <8 x float> %YMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = load i64, i64* %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX_init = trunc i64 %RCX_init to i32
%ECX = alloca i32
store i32 %ECX_init, i32* %ECX
%10 = lshr i64 %RCX_init, 8
br label  %bb_0
exit_fn_4004D0:                                   ; preds =  %bb_7
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
; <label>:bb_0
%RIP_1 = 1 +4195536
%EIP_0 = trunc i64 %RIP_1 to i32
%RBP_0 = load i64, i64* %RBP
%RSP_0 = load i64, i64* %RSP
%23 = %RSP_0 -8
store i64 %RBP_0, i64* %23, align 1
%RSP_1 = %RSP_0 -8
%ESP_0 = trunc i64 %RSP_1 to i32
%RIP_2 = %RIP_1 +3
%EIP_1 = trunc i64 %RIP_2 to i32
%EBP_0 = trunc i64 %RSP_1 to i32
%RIP_3 = %RIP_2 +4
%EIP_2 = trunc i64 %RIP_3 to i32
%RSP_2 = %RSP_1 -48
%ESP_1 = trunc i64 %RSP_2 to i32
%RIP_4 = %RIP_3 +10
%EIP_3 = trunc i64 %RIP_4 to i32
%RIP_5 = %RIP_4 +8
%EIP_4 = trunc i64 %RIP_5 to i32
%25 = %RIP_5 +430
%26 = load double, double* %25, align 1
%27 = bitcast double %26 to i64
%ZMM0_0 = load <16 x float>, <16 x float>* %ZMM0
%28 = bitcast <16 x float> %ZMM0_0 to i512
%XMM0_0 = trunc i512 %28 to i128
%XMM0_1 = %XMM0_0 : %27
%31 = bitcast <16 x float> %ZMM0_0 to i512
%YMM0_0 = trunc i512 %31 to i256
%YMM0_1 = %YMM0_0 : %XMM0_1
%34 = bitcast <16 x float> %ZMM0_0 to i512
%ZMM0_1 = %34 : %XMM0_1
%RIP_6 = %RIP_5 +7
%EIP_5 = trunc i64 %RIP_6 to i32
%38 = %RSP_1 -4
store i32 0, i32* %38, align 1
%RIP_7 = %RIP_6 +7
%EIP_6 = trunc i64 %RIP_7 to i32
%40 = %RSP_1 -8
store i32 5, i32* %40, align 1
%RIP_8 = %RIP_7 +5
%EIP_7 = trunc i64 %RIP_8 to i32
%41 = trunc i128 %XMM0_1 to i64
%42 = bitcast i64 %41 to double
%44 = %RSP_1 -16
store double %42, double* %44, align 1
%RIP_9 = %RIP_8 +7
%EIP_8 = trunc i64 %RIP_9 to i32
%46 = %RSP_1 -20
store i32 100, i32* %46, align 1
%RIP_10 = %RIP_9 +3
%EIP_9 = trunc i64 %RIP_10 to i32
%48 = %RSP_1 -20
%ESI_0 = load i32, i32* %48, align 1
%RSI_0 = load i64, i64* %RSI
%RSI_1 = zext i32 %ESI_0 to i64
%RIP_11 = %RIP_10 +5
%EIP_10 = trunc i64 %RIP_11 to i32
%50 = %RSP_1 -16
%51 = load double, double* %50, align 1
%52 = bitcast double %51 to i64
%XMM0_2 = %XMM0_1 : %52
%YMM0_2 = %YMM0_1 : %XMM0_2
%ZMM0_2 = %ZMM0_1 : %XMM0_2
%RIP_12 = %RIP_11 +2
%EIP_11 = trunc i64 %RIP_12 to i32
%RAX_0 = load i64, i64* %RAX
%EAX_0 = trunc i64 %RAX_0 to i32
%60 = and i32 %EAX_0, -256
%EAX_1 = or i32 1, %60
%61 = and i64 %RAX_0, -256
%RAX_1 = or i64 1, %61
%RIP_13 = %RIP_12 +5
%EIP_12 = trunc i64 %RIP_13 to i32
%62 = %RSP_2 -8
store i64 4195603, i64* %62
%ESP_2 = trunc i64 %RSP_3 to i32
store i32 %EAX_1, i32* %EAX
store i32 %EBP_0, i32* %EBP
store i32 4196000, i32* %EDI
%ZF_0 = icmp eq i64 %RSP_2, 0
%SF_0 = icmp slt i64 %RSP_2, 0
%63 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP_1, i64 48)
%OF_0 = extractvalue { i64, i1 } %63, 1
%64 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP_1, i64 48)
%CF_0 = extractvalue { i64, i1 } %64, 1
%65 = trunc i64 %RSP_2 to i8
%66 = call i8 @llvm.ctpop.i8(i8 %65)
%67 = trunc i8 %66 to i1
%PF_0 = icmp eq i1 %67, false
%68 = zext i1 %CF_0 to i32
%69 = shl i32 %68, 0
%71 = zext i1 %PF_0 to i32
%72 = shl i32 %71, 2
%74 = zext i1 false to i32
%75 = shl i32 %74, 4
%77 = zext i1 %ZF_0 to i32
%78 = shl i32 %77, 6
%80 = zext i1 %SF_0 to i32
%81 = shl i32 %80, 7
%83 = zext i1 %OF_0 to i32
%84 = shl i32 %83, 11
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
store <4 x float> %85, <4 x float>* %XMM0
%86 = bitcast i256 %YMM0_2 to <8 x float>
store <8 x float> %86, <8 x float>* %YMM0
%87 = bitcast i512 %ZMM0_2 to <16 x float>
store <16 x float> %87, <16 x float>* %ZMM0
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
%RIP_14 = load i64, i64* %RIP
%RIP_15 = %RIP_14 +7
%EIP_13 = trunc i64 %RIP_15 to i32
%RBP_1 = load i64, i64* %RBP
%111 = %RBP_1 -24
store i32 0, i32* %111, align 1
%RIP_16 = %RIP_15 +3
%EIP_14 = trunc i64 %RIP_16 to i32
%RAX_2 = load i64, i64* %RAX
%EAX_2 = trunc i64 %RAX_2 to i32
%113 = %RBP_1 -36
store i32 %EAX_2, i32* %113, align 1
store i32 %EAX_2, i32* %EAX
store i32 %EIP_14, i32* %EIP
store i64 %RAX_2, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 %RIP_16, i64* %RIP
br label  %bb_1
; <label>:bb_1
%RIP_85 = 3 +4195613
%EIP_73 = trunc i64 %RIP_85 to i32
%RBP_10 = load i64, i64* %RBP
%115 = %RBP_10 -24
%EAX_18 = load i32, i32* %115, align 1
%RAX_22 = load i64, i64* %RAX
%RAX_23 = zext i32 %EAX_18 to i64
%116 = lshr i32 %EAX_18, 8
%RIP_86 = %RIP_85 +3
%EIP_74 = trunc i64 %RIP_86 to i32
%118 = %RBP_10 -8
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
%120 = sub i32 %EAX_18, %119
%ZF_015 = icmp eq i32 %120, 0
%SF_016 = icmp slt i32 %120, 0
%121 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %EAX_18, i32 %119)
%OF_017 = extractvalue { i32, i1 } %121, 1
%122 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %EAX_18, i32 %119)
%CF_018 = extractvalue { i32, i1 } %122, 1
%123 = trunc i32 %120 to i8
%124 = call i8 @llvm.ctpop.i8(i8 %123)
%125 = trunc i8 %124 to i1
%PF_019 = icmp eq i1 %125, false
%126 = zext i1 %CF_018 to i32
%127 = shl i32 %126, 0
%129 = zext i1 %PF_019 to i32
%130 = shl i32 %129, 2
%132 = zext i1 false to i32
%133 = shl i32 %132, 4
%135 = zext i1 %ZF_015 to i32
%136 = shl i32 %135, 6
%138 = zext i1 %SF_016 to i32
%139 = shl i32 %138, 7
%141 = zext i1 %OF_017 to i32
%142 = shl i32 %141, 11
%RIP_87 = %RIP_86 +6
%EIP_75 = trunc i64 %RIP_87 to i32
store i32 %EAX_18, i32* %EAX
store i32 4195819, i32* %EIP
store i64 %RAX_23, i64* %RAX
store i64 %RBP_10, i64* %RBP
store i64 4195819, i64* %RIP
br i1 %CC_GE_0, label  %bb_4005EB, label  %bb_2
; <label>:bb_2
%RIP_18 = 4195625 +5
%EIP_15 = trunc i64 %RIP_18 to i32
%RBP_2 = load i64, i64* %RBP
%144 = %RBP_2 -16
%145 = load double, double* %144, align 1
%146 = bitcast double %145 to i64
%ZMM0_3 = load <16 x float>, <16 x float>* %ZMM0
%147 = bitcast <16 x float> %ZMM0_3 to i512
%XMM0_3 = trunc i512 %147 to i128
%XMM0_4 = %XMM0_3 : %146
%150 = bitcast <16 x float> %ZMM0_3 to i512
%YMM0_3 = trunc i512 %150 to i256
%YMM0_4 = %YMM0_3 : %XMM0_4
%153 = bitcast <16 x float> %ZMM0_3 to i512
%ZMM0_4 = %153 : %XMM0_4
%RIP_19 = %RIP_18 +3
%EIP_16 = trunc i64 %RIP_19 to i32
%157 = %RBP_2 -8
%EAX_3 = load i32, i32* %157, align 1
%RAX_3 = load i64, i64* %RAX
%RAX_4 = zext i32 %EAX_3 to i64
%158 = lshr i32 %EAX_3, 8
%RIP_20 = %RIP_19 +4
%EIP_17 = trunc i64 %RIP_20 to i32
%159 = sitofp i32 %EAX_3 to double
%160 = bitcast double %159 to i64
%ZMM1_0 = load <16 x float>, <16 x float>* %ZMM1
%161 = bitcast <16 x float> %ZMM1_0 to i512
%XMM1_0 = trunc i512 %161 to i128
%XMM1_1 = %XMM1_0 : %160
%164 = bitcast <16 x float> %ZMM1_0 to i512
%YMM1_0 = trunc i512 %164 to i256
%YMM1_1 = %YMM1_0 : %XMM1_1
%167 = bitcast <16 x float> %ZMM1_0 to i512
%ZMM1_1 = %167 : %XMM1_1
%RIP_21 = %RIP_20 +4
%EIP_18 = trunc i64 %RIP_21 to i32
%170 = trunc i128 %XMM0_4 to i64
%171 = bitcast i64 %170 to double
%172 = trunc i128 %XMM1_1 to i64
%173 = bitcast i64 %172 to double
%174 = fdiv double %171, %173
%175 = bitcast double %174 to i64
%XMM0_5 = %XMM0_4 : %175
%YMM0_5 = %YMM0_4 : %XMM0_5
%ZMM0_5 = %ZMM0_4 : %XMM0_5
%RIP_22 = %RIP_21 +5
%EIP_19 = trunc i64 %RIP_22 to i32
%182 = trunc i128 %XMM0_5 to i64
%183 = bitcast i64 %182 to double
%185 = %RBP_2 -32
store double %183, double* %185, align 1
%RIP_23 = %RIP_22 +5
%EIP_20 = trunc i64 %RIP_23 to i32
%187 = %RBP_2 -32
%188 = load double, double* %187, align 1
%189 = bitcast double %188 to i64
%XMM0_6 = %XMM0_5 : %189
%YMM0_6 = %YMM0_5 : %XMM0_6
%ZMM0_6 = %ZMM0_5 : %XMM0_6
%RIP_24 = %RIP_23 +3
%EIP_21 = trunc i64 %RIP_24 to i32
%197 = %RBP_2 -20
%EAX_4 = load i32, i32* %197, align 1
%RAX_5 = zext i32 %EAX_4 to i64
%198 = lshr i32 %EAX_4, 8
%RIP_25 = %RIP_24 +4
%EIP_22 = trunc i64 %RIP_25 to i32
%199 = sitofp i32 %EAX_4 to double
%200 = bitcast double %199 to i64
%XMM1_2 = %XMM1_1 : %200
%YMM1_2 = %YMM1_1 : %XMM1_2
%ZMM1_2 = %ZMM1_1 : %XMM1_2
%RIP_26 = %RIP_25 +4
%EIP_23 = trunc i64 %RIP_26 to i32
%207 = trunc i128 %XMM1_2 to i64
%208 = bitcast i64 %207 to double
%209 = trunc i128 %XMM0_6 to i64
%210 = bitcast i64 %209 to double
%ZF_01 = fcmp ueq double %208, %210
%PF_02 = fcmp uno double %208, %210
%CF_03 = fcmp ult double %208, %210
%211 = zext i1 %CF_03 to i32
%212 = shl i32 %211, 0
%214 = zext i1 %PF_02 to i32
%215 = shl i32 %214, 2
%217 = zext i1 false to i32
%218 = shl i32 %217, 4
%220 = zext i1 %ZF_01 to i32
%221 = shl i32 %220, 6
%223 = zext i1 false to i32
%224 = shl i32 %223, 7
%226 = zext i1 false to i32
%227 = shl i32 %226, 11
%RIP_27 = %RIP_26 +6
%EIP_24 = trunc i64 %RIP_27 to i32
%CC_BE_0 = or i1 %CF_03, %ZF_01
store i32 %EAX_4, i32* %EAX
store i32 4195721, i32* %EIP
store i64 %RAX_5, i64* %RAX
store i64 %RBP_2, i64* %RBP
store i64 4195721, i64* %RIP
%228 = bitcast i128 %XMM0_6 to <4 x float>
store <4 x float> %228, <4 x float>* %XMM0
%229 = bitcast i128 %XMM1_2 to <4 x float>
store <4 x float> %229, <4 x float>* %XMM1
%230 = bitcast i256 %YMM0_6 to <8 x float>
store <8 x float> %230, <8 x float>* %YMM0
%231 = bitcast i256 %YMM1_2 to <8 x float>
store <8 x float> %231, <8 x float>* %YMM1
%232 = bitcast i512 %ZMM0_6 to <16 x float>
store <16 x float> %232, <16 x float>* %ZMM0
%233 = bitcast i512 %ZMM1_2 to <16 x float>
store <16 x float> %233, <16 x float>* %ZMM1
br i1 %CC_BE_0, label  %bb_400589, label  %bb_3
; <label>:bb_3
%RIP_42 = 10 +4195668
%EIP_36 = trunc i64 %RIP_42 to i32
%RIP_43 = %RIP_42 +5
%EIP_37 = trunc i64 %RIP_43 to i32
%RBP_5 = load i64, i64* %RBP
%235 = %RBP_5 -32
%236 = load double, double* %235, align 1
%237 = bitcast double %236 to i64
%ZMM0_7 = load <16 x float>, <16 x float>* %ZMM0
%238 = bitcast <16 x float> %ZMM0_7 to i512
%XMM0_7 = trunc i512 %238 to i128
%XMM0_8 = %XMM0_7 : %237
%241 = bitcast <16 x float> %ZMM0_7 to i512
%YMM0_7 = trunc i512 %241 to i256
%YMM0_8 = %YMM0_7 : %XMM0_8
%244 = bitcast <16 x float> %ZMM0_7 to i512
%ZMM0_8 = %244 : %XMM0_8
%RIP_44 = %RIP_43 +5
%EIP_38 = trunc i64 %RIP_44 to i32
%247 = trunc i128 %XMM0_8 to i64
%248 = bitcast i64 %247 to double
%250 = %RBP_5 -16
%251 = load double, double* %250, align 1
%252 = fadd double %248, %251
%253 = bitcast double %252 to i64
%XMM0_9 = %XMM0_8 : %253
%YMM0_9 = %YMM0_8 : %XMM0_9
%ZMM0_9 = %ZMM0_8 : %XMM0_9
%RIP_45 = %RIP_44 +5
%EIP_39 = trunc i64 %RIP_45 to i32
%260 = trunc i128 %XMM0_9 to i64
%261 = bitcast i64 %260 to double
%263 = %RBP_5 -16
store double %261, double* %263, align 1
%RIP_46 = %RIP_45 +5
%EIP_40 = trunc i64 %RIP_46 to i32
%265 = %RBP_5 -32
%266 = load double, double* %265, align 1
%267 = bitcast double %266 to i64
%XMM0_10 = %XMM0_9 : %267
%YMM0_10 = %YMM0_9 : %XMM0_10
%ZMM0_10 = %ZMM0_9 : %XMM0_10
%RIP_47 = %RIP_46 +3
%EIP_41 = trunc i64 %RIP_47 to i32
%275 = %RBP_5 -20
%ESI_1 = load i32, i32* %275, align 1
%RSI_2 = load i64, i64* %RSI
%RSI_3 = zext i32 %ESI_1 to i64
%RIP_48 = %RIP_47 +5
%EIP_42 = trunc i64 %RIP_48 to i32
%277 = %RBP_5 -16
%278 = load double, double* %277, align 1
%279 = bitcast double %278 to i64
%ZMM1_3 = load <16 x float>, <16 x float>* %ZMM1
%280 = bitcast <16 x float> %ZMM1_3 to i512
%XMM1_3 = trunc i512 %280 to i128
%XMM1_4 = %XMM1_3 : %279
%283 = bitcast <16 x float> %ZMM1_3 to i512
%YMM1_3 = trunc i512 %283 to i256
%YMM1_4 = %YMM1_3 : %XMM1_4
%286 = bitcast <16 x float> %ZMM1_3 to i512
%ZMM1_4 = %286 : %XMM1_4
%RIP_49 = %RIP_48 +2
%EIP_43 = trunc i64 %RIP_49 to i32
%RAX_10 = load i64, i64* %RAX
%EAX_8 = trunc i64 %RAX_10 to i32
%290 = and i32 %EAX_8, -256
%EAX_9 = or i32 2, %290
%291 = and i64 %RAX_10, -256
%RAX_11 = or i64 2, %291
%RIP_50 = %RIP_49 +5
%EIP_44 = trunc i64 %RIP_50 to i32
%RSP_10 = load i64, i64* %RSP
%292 = %RSP_10 -8
store i64 4195713, i64* %292
%ESP_7 = trunc i64 %RSP_11 to i32
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
store <4 x float> %293, <4 x float>* %XMM0
%294 = bitcast i128 %XMM1_4 to <4 x float>
store <4 x float> %294, <4 x float>* %XMM1
%295 = bitcast i256 %YMM0_10 to <8 x float>
store <8 x float> %295, <8 x float>* %YMM0
%296 = bitcast i256 %YMM1_4 to <8 x float>
store <8 x float> %296, <8 x float>* %YMM1
%297 = bitcast i512 %ZMM0_10 to <16 x float>
store <16 x float> %297, <16 x float>* %ZMM0
%298 = bitcast i512 %ZMM1_4 to <16 x float>
store <16 x float> %298, <16 x float>* %ZMM1
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
%RIP_51 = load i64, i64* %RIP
%RIP_52 = %RIP_51 +3
%EIP_45 = trunc i64 %RIP_52 to i32
%RAX_12 = load i64, i64* %RAX
%EAX_10 = trunc i64 %RAX_12 to i32
%RBP_6 = load i64, i64* %RBP
%322 = %RBP_6 -40
store i32 %EAX_10, i32* %322, align 1
%RIP_53 = %RIP_52 +5
%EIP_46 = trunc i64 %RIP_53 to i32
store i32 %EAX_10, i32* %EAX
store i32 4195800, i32* %EIP
store i64 %RAX_12, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195800, i64* %RIP
br label  %bb_5
; <label>:bb_4
%RIP_56 = 10 +4195721
%EIP_48 = trunc i64 %RIP_56 to i32
%RIP_57 = %RIP_56 +5
%EIP_49 = trunc i64 %RIP_57 to i32
%RBP_7 = load i64, i64* %RBP
%324 = %RBP_7 -32
%325 = load double, double* %324, align 1
%326 = bitcast double %325 to i64
%ZMM0_11 = load <16 x float>, <16 x float>* %ZMM0
%327 = bitcast <16 x float> %ZMM0_11 to i512
%XMM0_11 = trunc i512 %327 to i128
%XMM0_12 = %XMM0_11 : %326
%330 = bitcast <16 x float> %ZMM0_11 to i512
%YMM0_11 = trunc i512 %330 to i256
%YMM0_12 = %YMM0_11 : %XMM0_12
%333 = bitcast <16 x float> %ZMM0_11 to i512
%ZMM0_12 = %333 : %XMM0_12
%RIP_58 = %RIP_57 +3
%EIP_50 = trunc i64 %RIP_58 to i32
%337 = %RBP_7 -20
%EAX_11 = load i32, i32* %337, align 1
%RAX_13 = load i64, i64* %RAX
%RAX_14 = zext i32 %EAX_11 to i64
%338 = lshr i32 %EAX_11, 8
%RIP_59 = %RIP_58 +4
%EIP_51 = trunc i64 %RIP_59 to i32
%339 = sitofp i32 %EAX_11 to double
%340 = bitcast double %339 to i64
%ZMM1_5 = load <16 x float>, <16 x float>* %ZMM1
%341 = bitcast <16 x float> %ZMM1_5 to i512
%XMM1_5 = trunc i512 %341 to i128
%XMM1_6 = %XMM1_5 : %340
%344 = bitcast <16 x float> %ZMM1_5 to i512
%YMM1_5 = trunc i512 %344 to i256
%YMM1_6 = %YMM1_5 : %XMM1_6
%347 = bitcast <16 x float> %ZMM1_5 to i512
%ZMM1_6 = %347 : %XMM1_6
%RIP_60 = %RIP_59 +4
%EIP_52 = trunc i64 %RIP_60 to i32
%350 = trunc i128 %XMM1_6 to i64
%351 = bitcast i64 %350 to double
%352 = trunc i128 %XMM0_12 to i64
%353 = bitcast i64 %352 to double
%354 = fadd double %351, %353
%355 = bitcast double %354 to i64
%XMM1_7 = %XMM1_6 : %355
%YMM1_7 = %YMM1_6 : %XMM1_7
%ZMM1_7 = %ZMM1_6 : %XMM1_7
%RIP_61 = %RIP_60 +4
%EIP_53 = trunc i64 %RIP_61 to i32
%362 = trunc i128 %XMM1_7 to i64
%363 = bitcast i64 %362 to double
%EAX_12 = fptosi double %363 to i32
%RAX_15 = zext i32 %EAX_12 to i64
%364 = lshr i32 %EAX_12, 8
%RIP_62 = %RIP_61 +3
%EIP_54 = trunc i64 %RIP_62 to i32
%366 = %RBP_7 -20
store i32 %EAX_12, i32* %366, align 1
%RIP_63 = %RIP_62 +5
%EIP_55 = trunc i64 %RIP_63 to i32
%368 = %RBP_7 -32
%369 = load double, double* %368, align 1
%EAX_13 = fptosi double %369 to i32
%RAX_16 = zext i32 %EAX_13 to i64
%370 = lshr i32 %EAX_13, 8
%RIP_64 = %RIP_63 +4
%EIP_56 = trunc i64 %RIP_64 to i32
%371 = sitofp i32 %EAX_13 to double
%372 = bitcast double %371 to i64
%XMM0_13 = %XMM0_12 : %372
%YMM0_13 = %YMM0_12 : %XMM0_13
%ZMM0_13 = %ZMM0_12 : %XMM0_13
%RIP_65 = %RIP_64 +5
%EIP_57 = trunc i64 %RIP_65 to i32
%380 = %RBP_7 -16
%381 = load double, double* %380, align 1
%382 = bitcast double %381 to i64
%XMM1_8 = %XMM1_7 : %382
%YMM1_8 = %YMM1_7 : %XMM1_8
%ZMM1_8 = %ZMM1_7 : %XMM1_8
%RIP_66 = %RIP_65 +4
%EIP_58 = trunc i64 %RIP_66 to i32
%389 = trunc i128 %XMM1_8 to i64
%390 = bitcast i64 %389 to double
%391 = trunc i128 %XMM0_13 to i64
%392 = bitcast i64 %391 to double
%393 = fsub double %390, %392
%394 = bitcast double %393 to i64
%XMM1_9 = %XMM1_8 : %394
%YMM1_9 = %YMM1_8 : %XMM1_9
%ZMM1_9 = %ZMM1_8 : %XMM1_9
%RIP_67 = %RIP_66 +5
%EIP_59 = trunc i64 %RIP_67 to i32
%401 = trunc i128 %XMM1_9 to i64
%402 = bitcast i64 %401 to double
%404 = %RBP_7 -16
store double %402, double* %404, align 1
%RIP_68 = %RIP_67 +5
%EIP_60 = trunc i64 %RIP_68 to i32
%406 = %RBP_7 -32
%407 = load double, double* %406, align 1
%408 = bitcast double %407 to i64
%XMM0_14 = %XMM0_13 : %408
%YMM0_14 = %YMM0_13 : %XMM0_14
%ZMM0_14 = %ZMM0_13 : %XMM0_14
%RIP_69 = %RIP_68 +3
%EIP_61 = trunc i64 %RIP_69 to i32
%416 = %RBP_7 -20
%ESI_2 = load i32, i32* %416, align 1
%RSI_4 = load i64, i64* %RSI
%RSI_5 = zext i32 %ESI_2 to i64
%RIP_70 = %RIP_69 +5
%EIP_62 = trunc i64 %RIP_70 to i32
%418 = %RBP_7 -16
%419 = load double, double* %418, align 1
%420 = bitcast double %419 to i64
%XMM1_10 = %XMM1_9 : %420
%YMM1_10 = %YMM1_9 : %XMM1_10
%ZMM1_10 = %ZMM1_9 : %XMM1_10
%RIP_71 = %RIP_70 +2
%EIP_63 = trunc i64 %RIP_71 to i32
%428 = and i32 %EAX_13, -256
%EAX_14 = or i32 2, %428
%429 = and i64 %RAX_16, -256
%RAX_17 = or i64 2, %429
%RIP_72 = %RIP_71 +5
%EIP_64 = trunc i64 %RIP_72 to i32
%RSP_12 = load i64, i64* %RSP
%430 = %RSP_12 -8
store i64 4195797, i64* %430
%ESP_8 = trunc i64 %RSP_13 to i32
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
store <4 x float> %431, <4 x float>* %XMM0
%432 = bitcast i128 %XMM1_10 to <4 x float>
store <4 x float> %432, <4 x float>* %XMM1
%433 = bitcast i256 %YMM0_14 to <8 x float>
store <8 x float> %433, <8 x float>* %YMM0
%434 = bitcast i256 %YMM1_10 to <8 x float>
store <8 x float> %434, <8 x float>* %YMM1
%435 = bitcast i512 %ZMM0_14 to <16 x float>
store <16 x float> %435, <16 x float>* %ZMM0
%436 = bitcast i512 %ZMM1_10 to <16 x float>
store <16 x float> %436, <16 x float>* %ZMM1
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
%RIP_73 = load i64, i64* %RIP
%RIP_74 = %RIP_73 +3
%EIP_65 = trunc i64 %RIP_74 to i32
%RAX_18 = load i64, i64* %RAX
%EAX_15 = trunc i64 %RAX_18 to i32
%RBP_8 = load i64, i64* %RBP
%460 = %RBP_8 -44
store i32 %EAX_15, i32* %460, align 1
store i32 %EAX_15, i32* %EAX
store i32 %EIP_65, i32* %EIP
store i64 %RAX_18, i64* %RAX
store i64 %RBP_8, i64* %RBP
store i64 %RIP_74, i64* %RIP
br label  %bb_5
; <label>:bb_5
%RIP_76 = 4195800 +5
%EIP_66 = trunc i64 %RIP_76 to i32
store i32 4195805, i32* %EIP
store i64 4195805, i64* %RIP
br label  %bb_6
; <label>:bb_6
%RIP_79 = 3 +4195805
%EIP_68 = trunc i64 %RIP_79 to i32
%RBP_9 = load i64, i64* %RBP
%462 = %RBP_9 -24
%EAX_16 = load i32, i32* %462, align 1
%RAX_19 = load i64, i64* %RAX
%RAX_20 = zext i32 %EAX_16 to i64
%463 = lshr i32 %EAX_16, 8
%RIP_80 = %RIP_79 +3
%EIP_69 = trunc i64 %RIP_80 to i32
%EAX_17 = %EAX_16 +1
%RAX_21 = zext i32 %EAX_17 to i64
%464 = lshr i32 %EAX_17, 8
%RIP_81 = %RIP_80 +3
%EIP_70 = trunc i64 %RIP_81 to i32
%466 = %RBP_9 -24
store i32 %EAX_17, i32* %466, align 1
%RIP_82 = %RIP_81 +5
%EIP_71 = trunc i64 %RIP_82 to i32
%ZF_09 = icmp eq i32 %EAX_17, 0
%SF_010 = icmp slt i32 %EAX_17, 0
%467 = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 %EAX_16, i32 1)
%OF_011 = extractvalue { i32, i1 } %467, 1
%468 = call { i32, i1 } @llvm.uadd.with.overflow.i32(i32 %EAX_16, i32 1)
%CF_012 = extractvalue { i32, i1 } %468, 1
%469 = trunc i32 %EAX_17 to i8
%470 = call i8 @llvm.ctpop.i8(i8 %469)
%471 = trunc i8 %470 to i1
%PF_013 = icmp eq i1 %471, false
%472 = zext i1 %CF_012 to i32
%473 = shl i32 %472, 0
%475 = zext i1 %PF_013 to i32
%476 = shl i32 %475, 2
%478 = zext i1 false to i32
%479 = shl i32 %478, 4
%481 = zext i1 %ZF_09 to i32
%482 = shl i32 %481, 6
%484 = zext i1 %SF_010 to i32
%485 = shl i32 %484, 7
%487 = zext i1 %OF_011 to i32
%488 = shl i32 %487, 11
store i32 %EAX_17, i32* %EAX
store i32 4195613, i32* %EIP
store i64 %RAX_21, i64* %RAX
store i64 %RBP_9, i64* %RBP
store i64 4195613, i64* %RIP
br label  %bb_1
; <label>:bb_7
%RIP_30 = 10 +4195819
%EIP_26 = trunc i64 %RIP_30 to i32
%RIP_31 = %RIP_30 +2
%EIP_27 = trunc i64 %RIP_31 to i32
%RAX_6 = load i64, i64* %RAX
%EAX_5 = trunc i64 %RAX_6 to i32
%490 = and i32 %EAX_5, -256
%EAX_6 = or i32 0, %490
%491 = and i64 %RAX_6, -256
%RAX_7 = or i64 0, %491
%RIP_32 = %RIP_31 +5
%EIP_28 = trunc i64 %RIP_32 to i32
%RSP_4 = load i64, i64* %RSP
%492 = %RSP_4 -8
store i64 4195836, i64* %492
%ESP_3 = trunc i64 %RSP_5 to i32
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
%RIP_33 = load i64, i64* %RIP
%RIP_34 = %RIP_33 +2
%EIP_29 = trunc i64 %RIP_34 to i32
%RCX_0 = load i64, i64* %RCX
%ECX_0 = trunc i64 %RCX_0 to i32
%ECX_1 = xor i32 %ECX_0, %ECX_0
%RCX_1 = zext i32 %ECX_1 to i64
%515 = lshr i32 %ECX_1, 8
%RIP_35 = %RIP_34 +3
%EIP_30 = trunc i64 %RIP_35 to i32
%RAX_8 = load i64, i64* %RAX
%EAX_7 = trunc i64 %RAX_8 to i32
%RBP_3 = load i64, i64* %RBP
%517 = %RBP_3 -48
store i32 %EAX_7, i32* %517, align 1
%RIP_36 = %RIP_35 +2
%EIP_31 = trunc i64 %RIP_36 to i32
%RAX_9 = zext i32 %ECX_1 to i64
%518 = lshr i32 %ECX_1, 8
%RIP_37 = %RIP_36 +4
%EIP_32 = trunc i64 %RIP_37 to i32
%RSP_6 = load i64, i64* %RSP
%RSP_7 = %RSP_6 +48
%ESP_4 = trunc i64 %RSP_7 to i32
%RIP_38 = %RIP_37 +1
%EIP_33 = trunc i64 %RIP_38 to i32
%RSP_8 = %RSP_7 +8
%ESP_5 = trunc i64 %RSP_8 to i32
%520 = %RSP_8 -8
%RBP_4 = load i64, i64* %520, align 1
%EBP_1 = trunc i64 %RBP_4 to i32
%RIP_39 = %RIP_38 +1
%EIP_34 = trunc i64 %RIP_39 to i32
%521 = %RSP_8 +8
%RIP_40 = load i64, i64* %521
%ESP_6 = trunc i64 %RSP_9 to i32
%EIP_35 = trunc i64 %RIP_40 to i32
%ZF_04 = icmp eq i64 %RSP_7, 0
%SF_05 = icmp slt i64 %RSP_7, 0
%522 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP_6, i64 48)
%OF_06 = extractvalue { i64, i1 } %522, 1
%523 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP_6, i64 48)
%CF_07 = extractvalue { i64, i1 } %523, 1
%524 = trunc i64 %RSP_7 to i8
%525 = call i8 @llvm.ctpop.i8(i8 %524)
%526 = trunc i8 %525 to i1
%PF_08 = icmp eq i1 %526, false
%527 = zext i1 %CF_07 to i32
%528 = shl i32 %527, 0
%530 = zext i1 %PF_08 to i32
%531 = shl i32 %530, 2
%533 = zext i1 false to i32
%534 = shl i32 %533, 4
%536 = zext i1 %ZF_04 to i32
%537 = shl i32 %536, 6
%539 = zext i1 %SF_05 to i32
%540 = shl i32 %539, 7
%542 = zext i1 %OF_06 to i32
%543 = shl i32 %542, 11
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

define void @fn_4003D0(%regset* noalias nocapture) {
entry_fn_4003D0:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = load i64, i64* %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP_init = trunc i64 %RIP_init to i32
%EIP = alloca i32
store i32 %EIP_init, i32* %EIP
br label  %bb_8
exit_fn_4003D0:                                   ; preds =  %bb_8
%1 = load i64, i64* %RIP
store i64 %1, i64* %RIP_ptr
ret void
; <label>:bb_8
%RIP_1 = 4195280 +6
%EIP_0 = trunc i64 %RIP_1 to i32
%3 = %RIP_1 +2100290
%RIP_2 = load i64, i64* %3, align 1
%EIP_1 = trunc i64 %RIP_2 to i32
%4 = inttoptr i64 %RIP_2 to i8*
%5 = call i8* @llvm.dc.translate.at(i8* %4)
%6 = bitcast i8* %5 to void (%regset*)*
store i32 %EIP_1, i32* %EIP
store i64 %RIP_2, i64* %RIP
%7 = load i64, i64* %RIP
store i64 %7, i64* %RIP_ptr
call void %6(%regset* %0)
%8 = load i64, i64* %RIP_ptr
store i64 %8, i64* %RIP
br label %exit_fn_4003D0
}

