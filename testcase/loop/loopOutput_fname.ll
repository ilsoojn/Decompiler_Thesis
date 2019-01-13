define void @main(%regset* noalias nocapture) {
; <label>:1:
%2 = alloca i64
%3 = alloca i32
%4 = alloca i64
%5 = alloca i64
%6 = alloca i32
%7 = alloca i32
%8 = alloca <16 x float>
%9 = alloca <4 x float>
%10 = alloca <8 x float>
%11 = alloca <16 x float>
%12 = alloca <4 x float>
%13 = alloca <8 x float>
%14 = alloca i64
%15 = alloca i32
%16 = alloca i32
%17 = alloca i64, align 8
%18 = alloca i32, align 4
%19 = alloca i32, align 4
%20 = alloca i32, align 4
%21 = alloca double, align 8
%22 = alloca double, align 8
%23 = alloca double, align 8
br label %24
; <label>:24:
store i64 %4, i64* %17, align 1
store i32 0, i32* %18, align 1
store i32 5, i32* %19, align 1
store i32 625, i32* %20, align 1
store double 5.5, double* %21, align 1
store double 1.2, double* %22, align 1
store i32 %17, i32* %7
store i32 4195507, i32* %3
store i32 %17, i32* %6
store i64 %17, i64* %4
store i64 4195507, i64* %2
store i64 %17, i64* %5
store <4 x float> 1.2, <4 x float>* %9
store <4 x float> 5.5, <4 x float>* %12
store <8 x float> 1.2, <8 x float>* %10
store <8 x float> 5.5, <8 x float>* %13
store <16 x float> 1.2, <16 x float>* %8
store <16 x float> 5.5, <16 x float>* %11
br label %25
; <label>:25:
%26 = load i32, i32* %20, align 1
%27 = icmp sge i32 %19, %26
%28 = load i32, i32* %16
store i32 %28, i32* %16
store i32 %19, i32* %15
store i32 4195531, i32* %3
store i64 %19, i64* %14
store i64 %17, i64* %4
store i64 4195531, i64* %2
br i1 %27, label %31, label %29
; <label>:29:
%30 = load i32, i32* %16
store i32 %30, i32* %16
store i32 4195507, i32* %3
store i64 %17, i64* %4
store i64 4195507, i64* %2
br label %25
; <label>:31:
store i32 4195536, i32* %3
store i64 4195536, i64* %2
br label %32
; <label>:32:
%33 = load double, double* %21, align 1
%34 = load double, double* %22, align 1
%35 = fcmp ult double %33, %34
%36 = load i32, i32* %16
store i32 %36, i32* %16
store i32 4195587, i32* %3
store i64 %17, i64* %4
store i64 4195587, i64* %2
store <4 x float> %33, <4 x float>* %9
store <8 x float> %33, <8 x float>* %10
store <16 x float> %33, <16 x float>* %8
br i1 %35, label %44, label %37
; <label>:37:
%38 = load double, double* %21, align 1
%39 = load double, double* %22, align 1
%40 = fdiv double %38, %39
store double %40, double* %23, align 1
%41 = load double, double* %23, align 1
%42 = load double, double* %22, align 1
%43 = fadd double %41, %42
store double %43, double* %22, align 1
store i32 4195536, i32* %3
store i64 %17, i64* %4
store i64 4195536, i64* %2
store <4 x float> %43, <4 x float>* %9
store <8 x float> %43, <8 x float>* %10
store <16 x float> %43, <16 x float>* %8
br label %32
; <label>:44:
%45 = load i32, i32* %16
store i32 %45, i32* %16
store i32 %17, i32* %7
store i32 %5, i32* %3
store i64 %17, i64* %4
store i64 %5, i64* %2
br label %46
; <label>:46:
ret void
}

