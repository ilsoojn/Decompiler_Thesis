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
  %RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
  %RCX_init = load i64, i64* %RCX_ptr
  %RCX = alloca i64
  store i64 %RCX_init, i64* %RCX
  %ECX_init = trunc i64 %RCX_init to i32
  %ECX = alloca i32
  store i32 %ECX_init, i32* %ECX
  %10 = lshr i64 %RCX_init, 8
  br label %bb_4004D0
exit_fn_4004D0:                                   ; preds = %bb_4005EB
  %13 = [%RAX]
  store i64 %13, i64* %RAX_ptr
  %14 = [%RBP]
  store i64 %14, i64* %RBP_ptr
  %15 = [%RCX]
  store i64 %15, i64* %RCX_ptr
  %16 = [%RDI]
  store i64 %16, i64* %RDI_ptr
  %17 = [%RIP]
  store i64 %17, i64* %RIP_ptr
  %18 = [%RSI]
  store i64 %18, i64* %RSI_ptr
  %19 = [%RSP]
  store i64 %19, i64* %RSP_ptr
  %20 = [%ZMM0]
  store <16 x float> %20, <16 x float>* %ZMM0_ptr
  %21 = [%ZMM1]
  store <16 x float> %21, <16 x float>* %ZMM1_ptr
  ret void
bb_4004D0:                                        ; preds = %entry_fn_4004D0
  %RIP_1 = 4195537
  %EIP_0 = 4195537
  %RBP_0 = %RBP
  %RSP_0 = %RSP
  %23 = %RSP-8
  store i64 %RBP, i64* %RSP-8, align 1
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
  %25 = 4195992
  %26 = [4195992]
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
  %38 = %RSP-12
  store i32 0, i32* %RSP-12, align 1
  %RIP_7 = 4195576
  %EIP_6 = 4195576
  %40 = %RSP-16
  store i32 5, i32* %RSP-16, align 1
  %RIP_8 = 4195581
  %EIP_7 = 4195581
  %41 = %26
  %42 = %26
  %44 = %RSP-24
  store double %26, double* %RSP-24, align 1
  %RIP_9 = 4195588
  %EIP_8 = 4195588
  %46 = %RSP-28
  store i32 100, i32* %RSP-28, align 1
  %RIP_10 = 4195591
  %EIP_9 = 4195591
  %48 = %RSP-28
  %ESI_0 = [%RSP-28]
  %RSI_0 = %RSI
  %RSI_1 = %RSP-28
  %RIP_11 = 4195596
  %EIP_10 = 4195596
  %50 = %RSP-24
  %51 = [%RSP-24]
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
  %62 = %RSP-64
  store i64 4195603, i64* %RSP-64
  %ESP_2 = %RSP-64
  store i32 %EAX_1, i32* %EAX
  store i32 %RSP-8, i32* %EBP
  store i32 4196000, i32* %EDI
  %ZF_0 = (%RSP-56==0)
  %SF_0 = (%RSP-56<0)
  %63 = call { i64, i1 } @llvm.ssub.with.overflow.i64(i64 %RSP-8, i64 48)
  %OF_0 = extractvalue { i64, i1 } %63, 1
  %64 = call { i64, i1 } @llvm.usub.with.overflow.i64(i64 %RSP-8, i64 48)
  %CF_0 = extractvalue { i64, i1 } %64, 1
  %65 = %RSP-56
  %66 = call i8 @llvm.ctpop.i8(i8 %RSP-56)
  %67 = %66
  %PF_0 = (%66==false)
  %68 = %64
  %69 = shl i32 %64, 0
  %71 = (%66==false)
  %72 = shl i32 (%66==false), 2
  %74 = false
  %75 = shl i32 false, 4
  %77 = (%RSP-56==0)
  %78 = shl i32 (%RSP-56==0), 6
  %80 = (%RSP-56<0)
  %81 = shl i32 (%RSP-56<0), 7
  %83 = %63
  %84 = shl i32 %63, 11
  store i32 4195603, i32* %EIP
  store i32 [%RSP-28], i32* %ESI
  store i32 %RSP-64, i32* %ESP
  store i64 %RAX_1, i64* %RAX
  store i64 %RSP-8, i64* %RBP
  store i64 4196000, i64* %RDI
  store i64 4195603, i64* %RIP
  store i64 %RSP-28, i64* %RSI
  store i64 %RSP_3, i64* %RSP
  %85 = %51
  store <4 x float> %51, <4 x float>* %XMM0
  %86 = %51
  store <8 x float> %51, <8 x float>* %YMM0
  %87 = %51
  store <16 x float> %51, <16 x float>* %ZMM0
  %90 = [%RAX]
  store i64 %90, i64* %RAX_ptr
  %91 = [%RBP]
  store i64 %91, i64* %RBP_ptr
  %92 = [%RCX]
  store i64 %92, i64* %RCX_ptr
  %93 = [%RDI]
  store i64 %93, i64* %RDI_ptr
  %94 = [%RIP]
  store i64 %94, i64* %RIP_ptr
  %95 = [%RSI]
  store i64 %95, i64* %RSI_ptr
  %96 = [%RSP]
  store i64 %96, i64* %RSP_ptr
  %97 = [%ZMM0]
  store <16 x float> %97, <16 x float>* %ZMM0_ptr
  %98 = [%ZMM1]
  store <16 x float> %98, <16 x float>* %ZMM1_ptr
  call void @fn_4003D0(%regset* %0)
  %101 = [%RAX_ptr]
  store i64 %101, i64* %RAX
  %102 = [%RBP_ptr]
  store i64 %102, i64* %RBP
  %103 = [%RCX_ptr]
  store i64 %103, i64* %RCX
  %104 = [%RDI_ptr]
  store i64 %104, i64* %RDI
  %105 = [%RIP_ptr]
  store i64 %105, i64* %RIP
  %106 = [%RSI_ptr]
  store i64 %106, i64* %RSI
  %107 = [%RSP_ptr]
  store i64 %107, i64* %RSP
  %108 = [%ZMM0_ptr]
  store <16 x float> %108, <16 x float>* %ZMM0
  %109 = [%ZMM1_ptr]
  store <16 x float> %109, <16 x float>* %ZMM1
  %RIP_14 = %RIP
  %RIP_15 = %RIP+7
  %EIP_13 = %RIP+7
  %RBP_1 = %RBP
  %111 = %RBP-24
  store i32 0, i32* %RBP-24, align 1
  %RIP_16 = %RIP+10
  %EIP_14 = %RIP+10
  %RAX_2 = %RAX
  %EAX_2 = %RAX
  %113 = %RBP-36
  store i32 %RAX, i32* %RBP-36, align 1
  store i32 %RAX, i32* %EAX
  store i32 %RIP+10, i32* %EIP
  store i64 %RAX, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 %RIP+10, i64* %RIP
  br label %bb_40051D
bb_40051D: ; preds = %bb_4005DD, %bb_4004D0
  %RIP_85 = 4195616
  %EIP_73 = 4195616
  %RBP_10 = %RBP
  %115 = %RBP-24
  %EAX_18 = [%RBP-24]
  %RAX_22 = %RAX
  %RAX_23 = %RBP-24
  %116 = lshr i32 [%RBP-24], 8
  %RIP_86 = 4195619
  %EIP_74 = 4195619
  %118 = %RBP-8
  %119 = [%RBP-8]
  %CC_A_0 = ([%RBP-24]>%119)
  %CC_AE_0 = ([%RBP-24]>=%119)
  %CC_B_0 = ([%RBP-24]<%119)
  %CC_BE_014 = ([%RBP-24]<=%119)
  %CC_L_0 = ([%RBP-24]<%119)
  %CC_LE_0 = ([%RBP-24]<=%119)
  %CC_G_0 = ([%RBP-24]>%119)
  %CC_GE_0 = ([%RBP-24]>=%119)
  %CC_E_0 = ([%RBP-24]==%119)
  %CC_NE_0 = ([%RBP-24]!=%119)
  %120 = %119-[%RBP-24]
  %ZF_015 = ([%RBP-24]==0)
  %SF_016 = ([%RBP-24]<0)
  %121 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 [%RBP-24], i32 %119)
  %OF_017 = extractvalue { i32, i1 } %121, 1
  %122 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 [%RBP-24], i32 %119)
  %CF_018 = extractvalue { i32, i1 } %122, 1
  %123 = [%RBP-24]
  %124 = call i8 @llvm.ctpop.i8(i8 [%RBP-24])
  %125 = %124
  %PF_019 = (%124==false)
  %126 = %122
  %127 = shl i32 %122, 0
  %129 = (%124==false)
  %130 = shl i32 (%124==false), 2
  %132 = false
  %133 = shl i32 false, 4
  %135 = ([%RBP-24]==0)
  %136 = shl i32 ([%RBP-24]==0), 6
  %138 = ([%RBP-24]<0)
  %139 = shl i32 ([%RBP-24]<0), 7
  %141 = %121
  %142 = shl i32 %121, 11
  %RIP_87 = 4195625
  %EIP_75 = 4195625
  store i32 [%RBP-24], i32* %EAX
  store i32 4195819, i32* %EIP
  store i64 %RBP-24, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 4195819, i64* %RIP
  br i1 ([%RBP-24]>=%119), label %bb_4005EB, label %bb_400529
bb_400529: ; preds = %bb_40051D
  %RIP_18 = 4195630
  %EIP_15 = 4195630
  %RBP_2 = %RBP
  %144 = %RBP-16
  %145 = [%RBP-16]
  %146 = %145
  %ZMM0_3 = %ZMM0
  %147 = %ZMM0
  %XMM0_3 = %ZMM0
  %XMM0_4 = %145
  %150 = %ZMM0
  %YMM0_3 = %ZMM0
  %YMM0_4 = %145
  %153 = %ZMM0
  %ZMM0_4 = %145
  %RIP_19 = 4195633
  %EIP_16 = 4195633
  %157 = %RBP-8
  %EAX_3 = [%RBP-8]
  %RAX_3 = %RAX
  %RAX_4 = %RBP-8
  %158 = lshr i32 [%RBP-8], 8
  %RIP_20 = 4195637
  %EIP_17 = 4195637
  %159 = [%RBP-8]
  %160 = [%RBP-8]
  %ZMM1_0 = %ZMM1
  %161 = %ZMM1
  %XMM1_0 = %ZMM1
  %XMM1_1 = [%RBP-8]
  %164 = %ZMM1
  %YMM1_0 = %ZMM1
  %YMM1_1 = [%RBP-8]
  %167 = %ZMM1
  %ZMM1_1 = [%RBP-8]
  %RIP_21 = 4195641
  %EIP_18 = 4195641
  %170 = %145
  %171 = %145
  %172 = [%RBP-8]
  %173 = [%RBP-8]
  %174 = fdiv double %145, [%RBP-8]
  %175 = %174
  %XMM0_5 = %174
  %YMM0_5 = %174
  %ZMM0_5 = %174
  %RIP_22 = 4195646
  %EIP_19 = 4195646
  %182 = %174
  %183 = %174
  %185 = %RBP-32
  store double %174, double* %RBP-32, align 1
  %RIP_23 = 4195651
  %EIP_20 = 4195651
  %187 = %RBP-32
  %188 = [%RBP-32]
  %189 = %188
  %XMM0_6 = %188
  %YMM0_6 = %188
  %ZMM0_6 = %188
  %RIP_24 = 4195654
  %EIP_21 = 4195654
  %197 = %RBP-20
  %EAX_4 = [%RBP-20]
  %RAX_5 = %RBP-20
  %198 = lshr i32 [%RBP-20], 8
  %RIP_25 = 4195658
  %EIP_22 = 4195658
  %199 = [%RBP-20]
  %200 = [%RBP-20]
  %XMM1_2 = [%RBP-20]
  %YMM1_2 = [%RBP-20]
  %ZMM1_2 = [%RBP-20]
  %RIP_26 = 4195662
  %EIP_23 = 4195662
  %207 = [%RBP-20]
  %208 = [%RBP-20]
  %209 = %188
  %210 = %188
  %ZF_01 = ([%RBP-20]==%188)
  %PF_02 = ([%RBP-20]%188)
  %CF_03 = ([%RBP-20]<%188)
  %211 = ([%RBP-20]<%188)
  %212 = shl i32 ([%RBP-20]<%188), 0
  %214 = ([%RBP-20]%188)
  %215 = shl i32 ([%RBP-20]%188), 2
  %217 = false
  %218 = shl i32 false, 4
  %220 = ([%RBP-20]==%188)
  %221 = shl i32 ([%RBP-20]==%188), 6
  %223 = false
  %224 = shl i32 false, 7
  %226 = false
  %227 = shl i32 false, 11
  %RIP_27 = 4195668
  %EIP_24 = 4195668
  %CC_BE_0 = or i1 ([%RBP-20]<%188), ([%RBP-20]==%188)
  store i32 [%RBP-20], i32* %EAX
  store i32 4195721, i32* %EIP
  store i64 %RBP-20, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 4195721, i64* %RIP
  %228 = %188
  store <4 x float> %188, <4 x float>* %XMM0
  %229 = [%RBP-20]
  store <4 x float> [%RBP-20], <4 x float>* %XMM1
  %230 = %188
  store <8 x float> %188, <8 x float>* %YMM0
  %231 = [%RBP-20]
  store <8 x float> [%RBP-20], <8 x float>* %YMM1
  %232 = %188
  store <16 x float> %188, <16 x float>* %ZMM0
  %233 = [%RBP-20]
  store <16 x float> [%RBP-20], <16 x float>* %ZMM1
  br i1 %CC_BE_0, label %bb_400589, label %bb_400554
bb_400554: ; preds = %bb_400529
  %RIP_42 = 4195678
  %EIP_36 = 4195678
  %RIP_43 = 4195683
  %EIP_37 = 4195683
  %RBP_5 = %RBP
  %235 = %RBP-32
  %236 = [%RBP-32]
  %237 = %236
  %ZMM0_7 = %ZMM0
  %238 = %ZMM0
  %XMM0_7 = %ZMM0
  %XMM0_8 = %236
  %241 = %ZMM0
  %YMM0_7 = %ZMM0
  %YMM0_8 = %236
  %244 = %ZMM0
  %ZMM0_8 = %236
  %RIP_44 = 4195688
  %EIP_38 = 4195688
  %247 = %236
  %248 = %236
  %250 = %RBP-16
  %251 = [%RBP-16]
  %252 = fadd double %236, %251
  %253 = %252
  %XMM0_9 = %252
  %YMM0_9 = %252
  %ZMM0_9 = %252
  %RIP_45 = 4195693
  %EIP_39 = 4195693
  %260 = %252
  %261 = %252
  %263 = %RBP-16
  store double %252, double* %RBP-16, align 1
  %RIP_46 = 4195698
  %EIP_40 = 4195698
  %265 = %RBP-32
  %266 = [%RBP-32]
  %267 = %266
  %XMM0_10 = %266
  %YMM0_10 = %266
  %ZMM0_10 = %266
  %RIP_47 = 4195701
  %EIP_41 = 4195701
  %275 = %RBP-20
  %ESI_1 = [%RBP-20]
  %RSI_2 = %RSI
  %RSI_3 = %RBP-20
  %RIP_48 = 4195706
  %EIP_42 = 4195706
  %277 = %RBP-16
  %278 = [%RBP-16]
  %279 = %278
  %ZMM1_3 = %ZMM1
  %280 = %ZMM1
  %XMM1_3 = %ZMM1
  %XMM1_4 = %278
  %283 = %ZMM1
  %YMM1_3 = %ZMM1
  %YMM1_4 = %278
  %286 = %ZMM1
  %ZMM1_4 = %278
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
  store i64 4195713, i64* %RSP-8
  %ESP_7 = %RSP-8
  store i32 %EAX_9, i32* %EAX
  store i32 4196027, i32* %EDI
  store i32 4195713, i32* %EIP
  store i32 [%RBP-20], i32* %ESI
  store i32 %RSP-8, i32* %ESP
  store i64 %RAX_11, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 4196027, i64* %RDI
  store i64 4195713, i64* %RIP
  store i64 %RBP-20, i64* %RSI
  store i64 %RSP_11, i64* %RSP
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
  %301 = [%RAX]
  store i64 %301, i64* %RAX_ptr
  %302 = [%RBP]
  store i64 %302, i64* %RBP_ptr
  %303 = [%RCX]
  store i64 %303, i64* %RCX_ptr
  %304 = [%RDI]
  store i64 %304, i64* %RDI_ptr
  %305 = [%RIP]
  store i64 %305, i64* %RIP_ptr
  %306 = [%RSI]
  store i64 %306, i64* %RSI_ptr
  %307 = [%RSP]
  store i64 %307, i64* %RSP_ptr
  %308 = [%ZMM0]
  store <16 x float> %308, <16 x float>* %ZMM0_ptr
  %309 = [%ZMM1]
  store <16 x float> %309, <16 x float>* %ZMM1_ptr
  call void @fn_4003D0(%regset* %0)
  %312 = [%RAX_ptr]
  store i64 %312, i64* %RAX
  %313 = [%RBP_ptr]
  store i64 %313, i64* %RBP
  %314 = [%RCX_ptr]
  store i64 %314, i64* %RCX
  %315 = [%RDI_ptr]
  store i64 %315, i64* %RDI
  %316 = [%RIP_ptr]
  store i64 %316, i64* %RIP
  %317 = [%RSI_ptr]
  store i64 %317, i64* %RSI
  %318 = [%RSP_ptr]
  store i64 %318, i64* %RSP
  %319 = [%ZMM0_ptr]
  store <16 x float> %319, <16 x float>* %ZMM0
  %320 = [%ZMM1_ptr]
  store <16 x float> %320, <16 x float>* %ZMM1
  %RIP_51 = %RIP
  %RIP_52 = %RIP+3
  %EIP_45 = %RIP+3
  %RAX_12 = %RAX
  %EAX_10 = %RAX
  %RBP_6 = %RBP
  %322 = %RBP-40
  store i32 %RAX, i32* %RBP-40, align 1
  %RIP_53 = %RIP+8
  %EIP_46 = %RIP+8
  store i32 %RAX, i32* %EAX
  store i32 4195800, i32* %EIP
  store i64 %RAX, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 4195800, i64* %RIP
  br label %bb_4005D8
bb_400589: ; preds = %bb_400529
  %RIP_56 = 4195731
  %EIP_48 = 4195731
  %RIP_57 = 4195736
  %EIP_49 = 4195736
  %RBP_7 = %RBP
  %324 = %RBP-32
  %325 = [%RBP-32]
  %326 = %325
  %ZMM0_11 = %ZMM0
  %327 = %ZMM0
  %XMM0_11 = %ZMM0
  %XMM0_12 = %325
  %330 = %ZMM0
  %YMM0_11 = %ZMM0
  %YMM0_12 = %325
  %333 = %ZMM0
  %ZMM0_12 = %325
  %RIP_58 = 4195739
  %EIP_50 = 4195739
  %337 = %RBP-20
  %EAX_11 = [%RBP-20]
  %RAX_13 = %RAX
  %RAX_14 = %RBP-20
  %338 = lshr i32 [%RBP-20], 8
  %RIP_59 = 4195743
  %EIP_51 = 4195743
  %339 = [%RBP-20]
  %340 = [%RBP-20]
  %ZMM1_5 = %ZMM1
  %341 = %ZMM1
  %XMM1_5 = %ZMM1
  %XMM1_6 = [%RBP-20]
  %344 = %ZMM1
  %YMM1_5 = %ZMM1
  %YMM1_6 = [%RBP-20]
  %347 = %ZMM1
  %ZMM1_6 = [%RBP-20]
  %RIP_60 = 4195747
  %EIP_52 = 4195747
  %350 = [%RBP-20]
  %351 = [%RBP-20]
  %352 = %325
  %353 = %325
  %354 = fadd double [%RBP-20], %325
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
  %366 = %RBP-20
  store i32 %XMM1_7, i32* %RBP-20, align 1
  %RIP_63 = 4195759
  %EIP_55 = 4195759
  %368 = %RBP-32
  %369 = [%RBP-32]
  %EAX_13 = %RBP-32
  %RAX_16 = %RBP-32
  %370 = lshr i32 %RBP-32, 8
  %RIP_64 = 4195763
  %EIP_56 = 4195763
  %371 = %RBP-32
  %372 = %RBP-32
  %XMM0_13 = %RBP-32
  %YMM0_13 = %RBP-32
  %ZMM0_13 = %RBP-32
  %RIP_65 = 4195768
  %EIP_57 = 4195768
  %380 = %RBP-16
  %381 = [%RBP-16]
  %382 = %381
  %XMM1_8 = %381
  %YMM1_8 = %381
  %ZMM1_8 = %381
  %RIP_66 = 4195772
  %EIP_58 = 4195772
  %389 = %381
  %390 = %381
  %391 = %RBP-32
  %392 = %RBP-32
  %393 = fsub double %381, %RBP-32
  %394 = %393
  %XMM1_9 = %393
  %YMM1_9 = %393
  %ZMM1_9 = %393
  %RIP_67 = 4195777
  %EIP_59 = 4195777
  %401 = %393
  %402 = %393
  %404 = %RBP-16
  store double %393, double* %RBP-16, align 1
  %RIP_68 = 4195782
  %EIP_60 = 4195782
  %406 = %RBP-32
  %407 = [%RBP-32]
  %408 = %407
  %XMM0_14 = %407
  %YMM0_14 = %407
  %ZMM0_14 = %407
  %RIP_69 = 4195785
  %EIP_61 = 4195785
  %416 = %RBP-20
  %ESI_2 = [%RBP-20]
  %RSI_4 = %RSI
  %RSI_5 = %RBP-20
  %RIP_70 = 4195790
  %EIP_62 = 4195790
  %418 = %RBP-16
  %419 = [%RBP-16]
  %420 = %419
  %XMM1_10 = %419
  %YMM1_10 = %419
  %ZMM1_10 = %419
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
  store i64 4195797, i64* %RSP-8
  %ESP_8 = %RSP-8
  store i32 %EAX_14, i32* %EAX
  store i32 4196027, i32* %EDI
  store i32 4195797, i32* %EIP
  store i32 [%RBP-20], i32* %ESI
  store i32 %RSP-8, i32* %ESP
  store i64 %RAX_17, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 4196027, i64* %RDI
  store i64 4195797, i64* %RIP
  store i64 %RBP-20, i64* %RSI
  store i64 %RSP_13, i64* %RSP
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
  %439 = [%RAX]
  store i64 %439, i64* %RAX_ptr
  %440 = [%RBP]
  store i64 %440, i64* %RBP_ptr
  %441 = [%RCX]
  store i64 %441, i64* %RCX_ptr
  %442 = [%RDI]
  store i64 %442, i64* %RDI_ptr
  %443 = [%RIP]
  store i64 %443, i64* %RIP_ptr
  %444 = [%RSI]
  store i64 %444, i64* %RSI_ptr
  %445 = [%RSP]
  store i64 %445, i64* %RSP_ptr
  %446 = [%ZMM0]
  store <16 x float> %446, <16 x float>* %ZMM0_ptr
  %447 = [%ZMM1]
  store <16 x float> %447, <16 x float>* %ZMM1_ptr
  call void @fn_4003D0(%regset* %0)
  %450 = [%RAX_ptr]
  store i64 %450, i64* %RAX
  %451 = [%RBP_ptr]
  store i64 %451, i64* %RBP
  %452 = [%RCX_ptr]
  store i64 %452, i64* %RCX
  %453 = [%RDI_ptr]
  store i64 %453, i64* %RDI
  %454 = [%RIP_ptr]
  store i64 %454, i64* %RIP
  %455 = [%RSI_ptr]
  store i64 %455, i64* %RSI
  %456 = [%RSP_ptr]
  store i64 %456, i64* %RSP
  %457 = [%ZMM0_ptr]
  store <16 x float> %457, <16 x float>* %ZMM0
  %458 = [%ZMM1_ptr]
  store <16 x float> %458, <16 x float>* %ZMM1
  %RIP_73 = %RIP
  %RIP_74 = %RIP+3
  %EIP_65 = %RIP+3
  %RAX_18 = %RAX
  %EAX_15 = %RAX
  %RBP_8 = %RBP
  %460 = %RBP-44
  store i32 %RAX, i32* %RBP-44, align 1
  store i32 %RAX, i32* %EAX
  store i32 %RIP+3, i32* %EIP
  store i64 %RAX, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 %RIP+3, i64* %RIP
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
  %RBP_9 = %RBP
  %462 = %RBP-24
  %EAX_16 = [%RBP-24]
  %RAX_19 = %RAX
  %RAX_20 = %RBP-24
  %463 = lshr i32 [%RBP-24], 8
  %RIP_80 = 4195811
  %EIP_69 = 4195811
  %EAX_17 = %RBP-23
  %RAX_21 = %RBP-23
  %464 = lshr i32 %RBP-23, 8
  %RIP_81 = 4195814
  %EIP_70 = 4195814
  %466 = %RBP-24
  store i32 %RBP-23, i32* %RBP-24, align 1
  %RIP_82 = 4195819
  %EIP_71 = 4195819
  %ZF_09 = (%RBP-23==0)
  %SF_010 = (%RBP-23<0)
  %467 = call { i32, i1 } @llvm.sadd.with.overflow.i32(i32 [%RBP-24], i32 1)
  %OF_011 = extractvalue { i32, i1 } %467, 1
  %468 = call { i32, i1 } @llvm.uadd.with.overflow.i32(i32 [%RBP-24], i32 1)
  %CF_012 = extractvalue { i32, i1 } %468, 1
  %469 = %RBP-23
  %470 = call i8 @llvm.ctpop.i8(i8 %RBP-23)
  %471 = %470
  %PF_013 = (%470==false)
  %472 = %468
  %473 = shl i32 %468, 0
  %475 = (%470==false)
  %476 = shl i32 (%470==false), 2
  %478 = false
  %479 = shl i32 false, 4
  %481 = (%RBP-23==0)
  %482 = shl i32 (%RBP-23==0), 6
  %484 = (%RBP-23<0)
  %485 = shl i32 (%RBP-23<0), 7
  %487 = %467
  %488 = shl i32 %467, 11
  store i32 %RBP-23, i32* %EAX
  store i32 4195613, i32* %EIP
  store i64 %RBP-23, i64* %RAX
  store i64 %RBP, i64* %RBP
  store i64 4195613, i64* %RIP
  br label %bb_40051D
bb_4005EB: ; preds = %bb_40051D
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
  store i64 4195836, i64* %RSP-8
  %ESP_3 = %RSP-8
  store i32 %EAX_6, i32* %EAX
  store i32 4196049, i32* %EDI
  store i32 4195836, i32* %EIP
  store i32 %RSP-8, i32* %ESP
  store i64 %RAX_7, i64* %RAX
  store i64 4196049, i64* %RDI
  store i64 4195836, i64* %RIP
  store i64 %RSP_5, i64* %RSP
  %495 = [%RAX]
  store i64 %495, i64* %RAX_ptr
  %496 = [%RBP]
  store i64 %496, i64* %RBP_ptr
  %497 = [%RCX]
  store i64 %497, i64* %RCX_ptr
  %498 = [%RDI]
  store i64 %498, i64* %RDI_ptr
  %499 = [%RIP]
  store i64 %499, i64* %RIP_ptr
  %500 = [%RSI]
  store i64 %500, i64* %RSI_ptr
  %501 = [%RSP]
  store i64 %501, i64* %RSP_ptr
  %502 = [%ZMM0]
  store <16 x float> %502, <16 x float>* %ZMM0_ptr
  %503 = [%ZMM1]
  store <16 x float> %503, <16 x float>* %ZMM1_ptr
  call void @fn_4003D0(%regset* %0)
  %506 = [%RAX_ptr]
  store i64 %506, i64* %RAX
  %507 = [%RBP_ptr]
  store i64 %507, i64* %RBP
  %508 = [%RCX_ptr]
  store i64 %508, i64* %RCX
  %509 = [%RDI_ptr]
  store i64 %509, i64* %RDI
  %510 = [%RIP_ptr]
  store i64 %510, i64* %RIP
  %511 = [%RSI_ptr]
  store i64 %511, i64* %RSI
  %512 = [%RSP_ptr]
  store i64 %512, i64* %RSP
  %513 = [%ZMM0_ptr]
  store <16 x float> %513, <16 x float>* %ZMM0
  %514 = [%ZMM1_ptr]
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
  store i32 %RAX, i32* %RBP-48, align 1
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
  %520 = %RSP+48
  %RBP_4 = [%RSP+64]
  %EBP_1 = %RSP+64
  %RIP_39 = %RIP+13
  %EIP_34 = %RIP+13
  %521 = %RSP+64
  %RIP_40 = [%RSP+56]
  %ESP_6 = %RSP+64
  %EIP_35 = %RSP+56
  %ZF_04 = (%RSP+48==0)
  %SF_05 = (%RSP+48<0)
  %522 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %RSP, i64 48)
  %OF_06 = extractvalue { i64, i1 } %522, 1
  %523 = call { i64, i1 } @llvm.uadd.with.overflow.i64(i64 %RSP, i64 48)
  %CF_07 = extractvalue { i64, i1 } %523, 1
  %524 = %RSP+48
  %525 = call i8 @llvm.ctpop.i8(i8 %RSP+48)
  %526 = %525
  %PF_08 = (%525==false)
  %527 = %523
  %528 = shl i32 %523, 0
  %530 = (%525==false)
  %531 = shl i32 (%525==false), 2
  %533 = false
  %534 = shl i32 false, 4
  %536 = (%RSP+48==0)
  %537 = shl i32 (%RSP+48==0), 6
  %539 = (%RSP+48<0)
  %540 = shl i32 (%RSP+48<0), 7
  %542 = %522
  %543 = shl i32 %522, 11
  store i32 %ECX_1, i32* %EAX
  store i32 %RSP+64, i32* %EBP
  store i32 %ECX_1, i32* %ECX
  store i32 %RSP+56, i32* %EIP
  store i32 %RSP+64, i32* %ESP
  store i64 %ECX_1, i64* %RAX
  store i64 [%RSP+64], i64* %RBP
  store i64 %ECX_1, i64* %RCX
  store i64 [%RSP+56], i64* %RIP
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
  br label %bb_4003D0
exit_fn_4003D0:                                   ; preds = %bb_4003D0
  %1 = [%RIP]
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
  %7 = [%RIP]
  store i64 %7, i64* %RIP_ptr
  call void %5(%regset* %0)
  %8 = [%RIP_ptr]
  store i64 %8, i64* %RIP
  br label %exit_fn_4003D0
}
