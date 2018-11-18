define void @fn_400480(%regset* noalias nocapture) {
  %j = alloca i32, align 4
  %i = alloca i32, align 4
  %h = alloca i32, align 4
  %g = alloca i32, align 4
  %f = alloca i32, align 4
  %e = alloca i32, align 4
  %d = alloca double, align 8
  %c = alloca double, align 8
  %b = alloca i32, align 4
  %a = alloca i64, align 8
entry_fn_400480:
  %RBP = alloca i64
  %e = alloca i32
  br label %bb_400480
exit_fn_400480: ; preds = %bb_40053F
  ret void
bb_400480: ; preds = %entry_fn_400480
  store i64 %RBP, i64* %a, align 1
  store i32 0, i32* %b, align 1
  store double -43.2, double* %c, align 1
  store double -32.1, double* %d, align 1
  %60 = load double, double* %c, align 1
  %72 = load double, double* %d, align 1
  %ZF_0 = fcmp ueq double %60, %72
  %PF_0 = fcmp uno double %60, %72
  %CF_0 = fcmp ult double %60, %72
  %CtlSysEFLAGS_0 = load i32, i32* %e
  %74 = shl i32 %CF_0, 0
  %75 = or i32 %74, %CtlSysEFLAGS_0
  %77 = shl i32 %PF_0, 2
  %78 = or i32 %77, %75
  %80 = shl i32 false, 4
  %81 = or i32 %80, %78
  %83 = shl i32 %ZF_0, 6
  %84 = or i32 %83, %81
  %86 = shl i32 false, 7
  %87 = or i32 %86, %84
  %89 = shl i32 false, 11
  %EFLAGS_0 = or i32 %87, %89
  %CC_NE_0 = xor i1 %ZF_0, true
  store i32 %CtlSysEFLAGS_0, i32* %e
  br i1 %CC_NE_0, label %bb_4004C7, label %bb_4004B5
bb_4004B5: ; preds = %bb_400480
  %96 = lshr i32 %EFLAGS_0, 2
  br i1 %96, label %bb_4004C7, label %bb_4004BB
bb_4004BB: ; preds = %bb_4004B5
  store i32 0, i32* %f, align 1
  br label %bb_4004F3
bb_4004C7: ; preds = %bb_4004B5, %bb_400480
  %101 = load double, double* %c, align 1
  %114 = load double, double* %d, align 1
  %ZF_02 = fcmp ueq double %114, %101
  %CF_04 = fcmp ult double %114, %101
  %CtlSysEFLAGS_1 = load i32, i32* %e
  %CC_BE_0 = or i1 %CF_04, %ZF_02
  store i32 %CtlSysEFLAGS_1, i32* %e
  br i1 %CC_BE_0, label %bb_4004E7, label %bb_4004DB
bb_4004DB: ; preds = %bb_4004C7
  store i32 -1, i32* %f, align 1
  br label %bb_4004EE
bb_4004E7: ; preds = %bb_4004C7
  store i32 1, i32* %f, align 1
  br label %bb_4004EE
bb_4004EE: ; preds = %bb_4004E7, %bb_4004DB
  br label %bb_4004F3
bb_4004F3: ; preds = %bb_4004EE, %bb_4004BB
  store i32 %f, i32* %g, align 1
  store i32 %RSP-35, i32* %h, align 1
  %165 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %f, i32 -1)
  %166 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %f, i32 -1)
  %168 = call i8 @llvm.ctpop.i8(i8 %RSP-35)
  %PF_07 = icmp eq i1 %168, false
  %CtlSysEFLAGS_2 = load i32, i32* %e
  %171 = shl i32 %166, 0
  %172 = or i32 %171, %CtlSysEFLAGS_2
  %174 = shl i32 %PF_07, 2
  %175 = or i32 %174, %172
  %177 = shl i32 false, 4
  %178 = or i32 %177, %175
  %180 = shl i32 %ZF_05, 6
  %181 = or i32 %180, %178
  %183 = shl i32 %SF_0, 7
  %184 = or i32 %183, %181
  %186 = shl i32 %165, 11
  %EFLAGS_4 = or i32 %184, %186
  %187 = lshr i32 %EFLAGS_4, 6
  store i32 %CtlSysEFLAGS_2, i32* %e
  br i1 %187, label %bb_400520, label %bb_400507
bb_400507: ; preds = %bb_4004F3
  br label %bb_40050C
bb_40050C: ; preds = %bb_400507
  store i32 %RSP-45, i32* %i, align 1
  %194 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %g, i32 1)
  %195 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %g, i32 1)
  %197 = call i8 @llvm.ctpop.i8(i8 %RSP-45)
  %PF_012 = icmp eq i1 %197, false
  %CtlSysEFLAGS_3 = load i32, i32* %e
  %200 = shl i32 %195, 0
  %201 = or i32 %200, %CtlSysEFLAGS_3
  %203 = shl i32 %PF_012, 2
  %204 = or i32 %203, %201
  %206 = shl i32 false, 4
  %207 = or i32 %206, %204
  %209 = shl i32 %ZF_08, 6
  %210 = or i32 %209, %207
  %212 = shl i32 %SF_09, 7
  %213 = or i32 %212, %210
  %215 = shl i32 %194, 11
  %EFLAGS_6 = or i32 %213, %215
  %216 = lshr i32 %EFLAGS_6, 6
store i32 %CtlSysEFLAGS_3, i32* %e
  br i1 %216, label %bb_40052C, label %bb_40051B
bb_40051B: ; preds = %bb_40050C
  br label %bb_400538
bb_400520: ; preds = %bb_4004F3
  store i32 -1, i32* %j, align 1
  br label %bb_40053F
bb_40052C: ; preds = %bb_40050C
  store i32 1, i32* %j, align 1
  br label %bb_40053F
bb_400538: ; preds = %bb_40051B
  store i32 0, i32* %j, align 1
  br label %bb_40053F
bb_40053F: ; preds = %bb_400538, %bb_40052C, %bb_400520
  br label %exit_fn_400480
}
