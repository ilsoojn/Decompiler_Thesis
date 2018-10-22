define void @fn_400550(%regset* noalias nocapture) {
entry_fn_400550:
%RBP = alloca i64
%ZMM0 = alloca <16 x float>
%RAX = alloca i64
br label %bb_400550
exit_fn_400550:                                   ; preds = %bb_4006BC
ret void
bb_400550:                                        ; preds = %entry_fn_400550
store i64 %RBP, i64* %RSP-8, align 1
store i32 0, i32* %RSP-12, align 1
store double 1336630430, double* %RSP-32, align 1
store double 4003244811, double* %RSP-24, align 1
%72 = [%RSP-24]
%84 = [%RSP-32]
%85 = fadd double %72, %84
store double %85, double* %RSP-40, align 1
%99 = [%RSP-24]
%111 = [%RSP-32]
%112 = fsub double %99, %111
store double %112, double* %RSP-48, align 1
%126 = [%RSP-24]
%138 = [%RSP-32]
%139 = fmul double %126, %138
store double %139, double* %RSP-56, align 1
%153 = [%RSP-24]
%165 = [%RSP-32]
%166 = fdiv double %153, %165
store double %166, double* %RSP-64, align 1
%180 = [%RSP-40]
%192 = [%RSP-48]
%193 = fmul double %180, %192
%205 = [%RSP-56]
%206 = fdiv double %193, %205
%218 = [%RSP-64]
%219 = fadd double %206, %218
store double %219, double* %RSP-72, align 1
%232 = %31
%235 = [%RSP-72]
%ZF_0 = fcmp ueq double %232, %235
%CF_0 = fcmp ult double %31, %235
%CC_BE_0 = or i1 %CF_0, %ZF_0
br i1 %CC_BE_0, label %bb_4005F7, label %bb_4005EA
bb_4005EA: ; preds = %bb_400550
store i32 %RBP-32, i32* %RBP-4, align 1
br label %bb_4006BC
bb_4005F7: ; preds = %bb_400550
%282 = [%RBP-64]
%CF_03 = fcmp ult double %282, %273
br i1 %CF_03, label %bb_400630, label %bb_400609
bb_400609: ; preds = %bb_4005F7
%332 = 5
%335 = [%RBP-64]
%ZF_07 = fcmp ueq double %332, %335
%CF_09 = fcmp ult double 5, %335
%CC_BE_010 = or i1 %CF_09, %ZF_07
br i1 %CC_BE_010, label %bb_400630, label %bb_400623
bb_400623: ; preds = %bb_400609
store i32 %RBP-40, i32* %RBP-4, align 1
br label %bb_4006BC
bb_400630: ; preds = %bb_400609, %bb_4005F7
%375 = [%RBP-64]
%387 = %375
%389 = 5
%ZF_011 = fcmp ueq double %387, %389
%CC_NE_0 = xor i1 %ZF_011, true
br i1 %CC_NE_0, label %bb_400661, label %bb_40064E
bb_40064E: ; preds = %bb_400630
bb_400654: ; preds = %bb_40064E
store i32 %RBP-48, i32* %RBP-4, align 1
br label %bb_4006BC
bb_400661: ; preds = %bb_40064E, %bb_400630
br label %bb_400666
bb_400666: ; preds = %bb_400678, %bb_400661
%434 = [%RBP-64]
%446 = %434
%448 = %425
%ZF_015 = fcmp ueq double %446, %448
%CF_017 = fcmp ult double %434, %425
%CC_BE_018 = or i1 %CF_017, %ZF_015
br i1 %CC_BE_018, label %bb_4006AB, label %bb_400678
bb_400678: ; preds = %bb_400666
store i64 4195980, i64* %RSP-8
call void @fn_400410(%regset* %0)
%526 = [%RBP-64]
%541 = fsub double %526, [4196184]
store double %541, double* %RBP-64, align 1
store i32 %RAX, i32* %RBP-68, align 1
br label %bb_400666
bb_4006AB: ; preds = %bb_400666
store i64 4196021, i64* %RSP-8
call void @fn_400530(%regset* %0)
store i32 %ZMM0, i32* %RBP-4, align 1
br label %bb_4006BC
bb_4006BC: ; preds = %bb_4006AB, %bb_400654, %bb_400623, %bb_4005EA
br label %exit_fn_400550
}

define void @fn_400410(%regset* noalias nocapture) {
entry_fn_400410:
br label %bb_400410
exit_fn_400410:                                   ; preds = %bb_400410
ret void
bb_400410:                                        ; preds = %entry_fn_400410
%5 = call i8* @llvm.dc.translate.at(i8* 6295576)
call void %5(%regset* %0)
br label %exit_fn_400410
}

define void @fn_400530(%regset* noalias nocapture) {
entry_fn_400530:
%ZMM0 = alloca <16 x float>
br label %bb_400530
exit_fn_400530:                                   ; preds = %bb_400530
ret void
bb_400530:                                        ; preds = %entry_fn_400530
store double %ZMM0, double* %RSP-16, align 1
br label %exit_fn_400530
}

