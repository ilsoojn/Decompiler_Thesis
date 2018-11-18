define void @fn_400480(%regset* noalias nocapture) {
; <label>:1:
%2 = alloca i64
%3 = alloca i64, align 8
%4 = alloca i32, align 4
%5 = alloca i32, align 4
%6 = alloca double, align 8
%7 = alloca double, align 8
%8 = alloca double, align 8
%9 = alloca i32, align 4
br label %10
; <label>:10:
store i64 %2, i64* %3, align 1
store i32 0, i32* %4, align 1
store i32 10, i32* %5, align 1
store double 313.24, double* %6, align 1
store double 10.0, double* %7, align 1
%11 = load double, double* %7, align 1
%12 = fmul double %5, %11
%13 = load double, double* %6, align 1
%14 = fadd double %12, %13
store double %14, double* %8, align 1
%15 = load i32, i32* %9
store i32 %15, i32* %9
br label %16
; <label>:16:
ret void
}

