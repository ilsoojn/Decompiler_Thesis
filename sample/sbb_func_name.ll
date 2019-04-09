define void @main(%regset* noalias nocapture) {
; <label>:1:
%2 = alloca i64
%3 = alloca i32
%4 = alloca i64
%5 = alloca i64
%6 = alloca i32
%7 = alloca i64
%8 = alloca i32
%9 = alloca <16 x float>
%10 = alloca <4 x float>
%11 = alloca <8 x float>
%12 = alloca <16 x float>
%13 = alloca <4 x float>
%14 = alloca <8 x float>
%15 = alloca i64
%16 = alloca i32
%17 = alloca i32
%18 = alloca i64, align 8
%19 = alloca i32, align 4
%20 = alloca i32, align 4
%21 = alloca double, align 8
%22 = alloca double, align 8
%23 = alloca double, align 8
br label %24
; <label>:24:
store i64 %4, i64* %18, align 1
%25 = %7
%26 = xor i32 %25, %25
store i32 0, i32* %19, align 1
store i32 10, i32* %20, align 1
store double 313.24, double* %21, align 1
store double 10.0, double* %22, align 1
%27 = load double, double* %22, align 1
%28 = fmul double %20, %27
%29 = load double, double* %21, align 1
%30 = fadd double %28, %29
store double %30, double* %23, align 1
%31 = load i32, i32* %17
store i32 %31, i32* %17
store i32 %26, i32* %8
store i32 %18, i32* %6
store i32 %20, i32* %16
store i32 %5, i32* %3
store i64 %26, i64* %7
store i64 %18, i64* %4
store i64 %20, i64* %15
store i64 %5, i64* %2
store <4 x float> %30, <4 x float>* %10
store <4 x float> 313.24, <4 x float>* %13
store <8 x float> %30, <8 x float>* %11
store <8 x float> 313.24, <8 x float>* %14
store <16 x float> %30, <16 x float>* %9
store <16 x float> 313.24, <16 x float>* %12
br label %32
; <label>:32:
ret void
}

