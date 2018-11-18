define void @fn_400480(%regset* noalias nocapture) {
%ak = alloca i16, align 2
%aj = alloca i32, align 4
%ai = alloca i8, align 1
%ah = alloca i64, align 8
%ag = alloca i32, align 4
%af = alloca i64, align 8
%ae = alloca i64, align 8
%ad = alloca double, align 8
%ac = alloca float, align 4
%ab = alloca i8, align 1
%k = alloca i8, align 1
%j = alloca i64, align 8
%i = alloca i64, align 8
%h = alloca i64, align 8
%g = alloca i64, align 8
%f = alloca i32, align 4
%e = alloca i32, align 4
%d = alloca i16, align 2
%c = alloca i16, align 2
%b = alloca i32, align 4
%a = alloca i64, align 8
entry_fn_400480:
%RBP = alloca i64
%ah = alloca i64
%aj = alloca i32
%ak = alloca i16
%ai = alloca i8
%ag = alloca i32
br label %bb_400480
exit_fn_400480: ; preds = %bb_400480
ret void
bb_400480: ; preds = %entry_fn_400480
store i64 %RBP, i64* %a, align 1
store i32 0, i32* %b, align 1
store i16 -32100, i16* %c, align 1
store i16 -104, i16* %d, align 1
store i32 -4967296, i32* %e, align 1
store i32 -2123123123, i32* %f, align 1
store i64 -2111111111, i64* %g, align 1
store i64 4200000000, i64* %h, align 1
store i64 -123123123, i64* %i, align 1
store i64 123123123123, i64* %j, align 1
store i8 32, i8* %k, align 1
store i8 33, i8* %ab, align 1
store float 1234.56787109375, float* %ac, align 1
store double 9876.54321, double* %ad, align 1
store i64 %f, i64* %ae, align 1
store i64 %ad, i64* %af, align 1
%CtlSysEFLAGS_0 = load i32, i32* %ag
store i32 %CtlSysEFLAGS_0, i32* %ag
store i64 4200000000, i64* %ah
store i8 0, i8* %ai
store i32 -94967296, i32* %aj
store i16 -5632, i16* %ak
br label %exit_fn_400480
}

