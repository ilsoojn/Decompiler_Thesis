define void @fn_4004D0(%regset* noalias nocapture) {
entry_fn_4004D0:
%RIP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 14
%RIP_init = load i64, i64* %RIP_ptr
%RIP = alloca i64
store i64 %RIP_init, i64* %RIP
%EIP = alloca i32
store i64 %RIP_init, i32* %EIP
%RBP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 9
%RBP_init = load i64, i64* %RBP_ptr
%RBP = alloca i64
store i64 %RBP_init, i64* %RBP
%RSP_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 16
%RSP_init = load i64, i64* %RSP_ptr
%RSP = alloca i64
store i64 %RSP_init, i64* %RSP
%ESP = alloca i32
store i64 %RSP_init, i32* %ESP
%EBP = alloca i32
store i64 %RBP_init, i32* %EBP
%RDI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 12
%RDI_init = load i64, i64* %RDI_ptr
%RDI = alloca i64
store i64 %RDI_init, i64* %RDI
%EDI = alloca i32
store i64 %RDI_init, i32* %EDI
%ZMM0_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 85
%ZMM0_init = load <16 x float>, <16 x float>* %ZMM0_ptr
%ZMM0 = alloca <16 x float>
store <16 x float> %ZMM0_init, <16 x float>* %ZMM0
%XMM0 = alloca <4 x float>
store <16 x float> %ZMM0_init, <4 x float>* %XMM0
%YMM0 = alloca <8 x float>
store <16 x float> %ZMM0_init, <8 x float>* %YMM0
%RSI_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 15
%RSI_init = load i64, i64* %RSI_ptr
%RSI = alloca i64
store i64 %RSI_init, i64* %RSI
%ESI = alloca i32
store i64 %RSI_init, i32* %ESI
%RAX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 8
%RAX_init = load i64, i64* %RAX_ptr
%RAX = alloca i64
store i64 %RAX_init, i64* %RAX
%EAX = alloca i32
store i64 %RAX_init, i32* %EAX
%ZMM1_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 86
%ZMM1_init = load <16 x float>, <16 x float>* %ZMM1_ptr
%ZMM1 = alloca <16 x float>
store <16 x float> %ZMM1_init, <16 x float>* %ZMM1
%XMM1 = alloca <4 x float>
store <16 x float> %ZMM1_init, <4 x float>* %XMM1
%YMM1 = alloca <8 x float>
store <16 x float> %ZMM1_init, <8 x float>* %YMM1
%RCX_ptr = getelementptr inbounds %regset, %regset* %0, i32 0, i32 11
%RCX_init = load i64, i64* %RCX_ptr
%RCX = alloca i64
store i64 %RCX_init, i64* %RCX
%ECX = alloca i32
store i64 %RCX_init, i32* %ECX
br label  %bb_0
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
%RBP_0 = load i64, i64* %RBP
%23 = %RSP_0-8
store i64 %RBP_0, i64* %23, align 1
%RSP_1 = %RSP_0-8
%25 = %RIP_5+430
%26 = load double, double* %25, align 1
%38 = %RSP_1-4
store i32 0, i32* %38, align 1
%40 = %RSP_1-8
store i32 5, i32* %40, align 1
%44 = %RSP_1-16
store double %26, double* %44, align 1
%46 = %RSP_1-20
store i32 100, i32* %46, align 1
%48 = %RSP_1-20
%ESI_0 = load i32, i32* %48, align 1
%50 = %RSP_1-16
%51 = load double, double* %50, align 1
%RAX_0 = load i64, i64* %RAX
%60 = and i64 %RAX_0, -256
%EAX_1 = or i32 1, %60
%61 = and i64 %RAX_0, -256
%RAX_1 = or i64 1, %61
%RIP_13 = %RIP_12+5
%62 = %RSP_2-8
store i64 4195603, i64* %62
store i32 %EAX_1, i32* %EAX
store i64 %RSP_1, i32* %EBP
store i32 4196000, i32* %EDI
store i64 %RIP_13, i32* %EIP
store i32 %ESI_0, i32* %ESI
store i64 %RSP_3, i32* %ESP
store i64 %RAX_1, i64* %RAX
store i64 %RSP_1, i64* %RBP
store i64 4196000, i64* %RDI
store i64 %RIP_13, i64* %RIP
store i32 %ESI_0, i64* %RSI
store i64 %RSP_3, i64* %RSP
store double %51, <4 x float>* %XMM0
store double %51, <8 x float>* %YMM0
store double %51, <16 x float>* %ZMM0
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
%RBP_1 = load i64, i64* %RBP
%111 = %RBP_1-24
store i32 0, i32* %111, align 1
%RIP_16 = %RIP_15+3
%RAX_2 = load i64, i64* %RAX
%113 = %RBP_1-36
store i64 %RAX_2, i32* %113, align 1
store i64 %RAX_2, i32* %EAX
store i64 %RIP_16, i32* %EIP
store i64 %RAX_2, i64* %RAX
store i64 %RBP_1, i64* %RBP
store i64 %RIP_16, i64* %RIP
br label  %bb_1
; <label>:bb_1
%RBP_10 = load i64, i64* %RBP
%115 = %RBP_10-24
%EAX_18 = load i32, i32* %115, align 1
%118 = %RBP_10-8
%119 = load i32, i32* %118, align 1
%CC_GE_0 = icmp sge i32 %EAX_18, %119
store i32 %EAX_18, i32* %EAX
store i32 4195819, i32* %EIP
store i32 %EAX_18, i64* %RAX
store i64 %RBP_10, i64* %RBP
store i64 4195819, i64* %RIP
br i1 %CC_GE_0, label  %bb_4005EB, label  %bb_2
; <label>:bb_2
%RBP_2 = load i64, i64* %RBP
%144 = %RBP_2-16
%145 = load double, double* %144, align 1
%157 = %RBP_2-8
%EAX_3 = load i32, i32* %157, align 1
%174 = fdiv i32 %145, %EAX_3
%185 = %RBP_2-32
store double %174, double* %185, align 1
%187 = %RBP_2-32
%188 = load double, double* %187, align 1
%197 = %RBP_2-20
%EAX_4 = load i32, i32* %197, align 1
%ZF_01 = fcmp ueq i32 %EAX_4, %188
%CF_03 = fcmp ult i32 %EAX_4, %188
%CC_BE_0 = or i1 %CF_03, %ZF_01
store i32 %EAX_4, i32* %EAX
store i32 4195721, i32* %EIP
store i32 %EAX_4, i64* %RAX
store i64 %RBP_2, i64* %RBP
store i64 4195721, i64* %RIP
store double %188, <4 x float>* %XMM0
store i32 %EAX_4, <4 x float>* %XMM1
store double %188, <8 x float>* %YMM0
store i32 %EAX_4, <8 x float>* %YMM1
store double %188, <16 x float>* %ZMM0
store i32 %EAX_4, <16 x float>* %ZMM1
br i1 %CC_BE_0, label  %bb_400589, label  %bb_3
; <label>:bb_3
%RBP_5 = load i64, i64* %RBP
%235 = %RBP_5-32
%236 = load double, double* %235, align 1
%250 = %RBP_5-16
%251 = load double, double* %250, align 1
%252 = fadd double %236, %251
%263 = %RBP_5-16
store double %252, double* %263, align 1
%265 = %RBP_5-32
%266 = load double, double* %265, align 1
%275 = %RBP_5-20
%ESI_1 = load i32, i32* %275, align 1
%277 = %RBP_5-16
%278 = load double, double* %277, align 1
%RAX_10 = load i64, i64* %RAX
%290 = and i64 %RAX_10, -256
%EAX_9 = or i32 2, %290
%291 = and i64 %RAX_10, -256
%RAX_11 = or i64 2, %291
%RIP_50 = %RIP_49+5
%292 = %RSP_10-8
store i64 4195713, i64* %292
store i32 %EAX_9, i32* %EAX
store i32 4196027, i32* %EDI
store i64 %RIP_50, i32* %EIP
store i32 %ESI_1, i32* %ESI
store i64 %RSP_11, i32* %ESP
store i64 %RAX_11, i64* %RAX
store i64 %RBP_5, i64* %RBP
store i64 4196027, i64* %RDI
store i64 %RIP_50, i64* %RIP
store i32 %ESI_1, i64* %RSI
store i64 %RSP_11, i64* %RSP
store double %266, <4 x float>* %XMM0
store double %278, <4 x float>* %XMM1
store double %266, <8 x float>* %YMM0
store double %278, <8 x float>* %YMM1
store double %266, <16 x float>* %ZMM0
store double %278, <16 x float>* %ZMM1
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
%RAX_12 = load i64, i64* %RAX
%RBP_6 = load i64, i64* %RBP
%322 = %RBP_6-40
store i64 %RAX_12, i32* %322, align 1
store i64 %RAX_12, i32* %EAX
store i32 4195800, i32* %EIP
store i64 %RAX_12, i64* %RAX
store i64 %RBP_6, i64* %RBP
store i64 4195800, i64* %RIP
br label  %bb_5
; <label>:bb_4
%RBP_7 = load i64, i64* %RBP
%324 = %RBP_7-32
%325 = load double, double* %324, align 1
%337 = %RBP_7-20
%EAX_11 = load i32, i32* %337, align 1
%354 = fadd i32 %EAX_11, %325
%366 = %RBP_7-20
store double %354, i32* %366, align 1
%368 = %RBP_7-32
%369 = load double, double* %368, align 1
%380 = %RBP_7-16
%381 = load double, double* %380, align 1
%393 = fsub double %381, %369
%404 = %RBP_7-16
store double %393, double* %404, align 1
%406 = %RBP_7-32
%407 = load double, double* %406, align 1
%416 = %RBP_7-20
%ESI_2 = load i32, i32* %416, align 1
%418 = %RBP_7-16
%419 = load double, double* %418, align 1
%428 = and double %369, -256
%EAX_14 = or i32 2, %428
%429 = and double %369, -256
%RAX_17 = or i64 2, %429
%RIP_72 = %RIP_71+5
%430 = %RSP_12-8
store i64 4195797, i64* %430
store i32 %EAX_14, i32* %EAX
store i32 4196027, i32* %EDI
store i64 %RIP_72, i32* %EIP
store i32 %ESI_2, i32* %ESI
store i64 %RSP_13, i32* %ESP
store i64 %RAX_17, i64* %RAX
store i64 %RBP_7, i64* %RBP
store i64 4196027, i64* %RDI
store i64 %RIP_72, i64* %RIP
store i32 %ESI_2, i64* %RSI
store i64 %RSP_13, i64* %RSP
store double %407, <4 x float>* %XMM0
store double %419, <4 x float>* %XMM1
store double %407, <8 x float>* %YMM0
store double %419, <8 x float>* %YMM1
store double %407, <16 x float>* %ZMM0
store double %419, <16 x float>* %ZMM1
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
%RIP_74 = %RIP_73+3
%RAX_18 = load i64, i64* %RAX
%RBP_8 = load i64, i64* %RBP
%460 = %RBP_8-44
store i64 %RAX_18, i32* %460, align 1
store i64 %RAX_18, i32* %EAX
store i64 %RIP_74, i32* %EIP
store i64 %RAX_18, i64* %RAX
store i64 %RBP_8, i64* %RBP
store i64 %RIP_74, i64* %RIP
br label  %bb_5
; <label>:bb_5
store i32 4195805, i32* %EIP
store i64 4195805, i64* %RIP
br label  %bb_6
; <label>:bb_6
%RBP_9 = load i64, i64* %RBP
%EAX_17 = %EAX_16+1
%466 = %RBP_9-24
store i32 %EAX_17, i32* %466, align 1
store i32 %EAX_17, i32* %EAX
store i32 4195613, i32* %EIP
store i32 %EAX_17, i64* %RAX
store i64 %RBP_9, i64* %RBP
store i64 4195613, i64* %RIP
br label  %bb_1
; <label>:bb_7
%RAX_6 = load i64, i64* %RAX
%490 = and i64 %RAX_6, -256
%EAX_6 = or i32 0, %490
%491 = and i64 %RAX_6, -256
%RAX_7 = or i64 0, %491
%RIP_32 = %RIP_31+5
%492 = %RSP_4-8
store i64 4195836, i64* %492
store i32 %EAX_6, i32* %EAX
store i32 4196049, i32* %EDI
store i64 %RIP_32, i32* %EIP
store i64 %RSP_5, i32* %ESP
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
%RCX_0 = load i64, i64* %RCX
%ECX_1 = xor i64 %RCX_0, %RCX_0
%RAX_8 = load i64, i64* %RAX
%517 = %RBP_3-48
store i64 %RAX_8, i32* %517, align 1
%520 = %RSP_8-8
%RBP_4 = load i64, i64* %520, align 1
%521 = %RSP_8+8
%RIP_40 = load i64, i64* %521
store i32 %ECX_1, i32* %EAX
store i64 %RBP_4, i32* %EBP
store i32 %ECX_1, i32* %ECX
store i64 %RIP_40, i32* %EIP
store i64 %RSP_9, i32* %ESP
store i32 %ECX_1, i64* %RAX
store i64 %RBP_4, i64* %RBP
store i32 %ECX_1, i64* %RCX
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
%EIP = alloca i32
store i64 %RIP_init, i32* %EIP
br label  %bb_8
%1 = load i64, i64* %RIP
store i64 %1, i64* %RIP_ptr
ret void
; <label>:bb_8
%3 = %RIP_1+2100290
%RIP_2 = load i64, i64* %3, align 1
%5 = call i64 @llvm.dc.translate.at(i8* %RIP_2)
store i64 %RIP_2, i32* %EIP
store i64 %RIP_2, i64* %RIP
%7 = load i64, i64* %RIP
store i64 %7, i64* %RIP_ptr
call void %5(%regset* %0)
%8 = load i64, i64* %RIP_ptr
store i64 %8, i64* %RIP
br label %exit_fn_4003D0
}

