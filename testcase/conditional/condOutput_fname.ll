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
%14 = alloca i32
%15 = alloca i64
%16 = alloca i32
%17 = alloca i64, align 8
%18 = alloca i32, align 4
%19 = alloca double, align 8
%20 = alloca double, align 8
%21 = alloca i32, align 4
%22 = alloca i32, align 4
%23 = alloca i32, align 4
br label %24
; <label>:24:
store i64 %4, i64* %17, align 1
store i32 0, i32* %18, align 1
store double -43.2, double* %19, align 1
store double -32.1, double* %20, align 1
%25 = load double, double* %19, align 1
%26 = load double, double* %20, align 1
%27 = fcmp ueq double %25, %26
%28 = load i32, i32* %14
%29 = xor i1 %27, true
store i32 %28, i32* %14
store i32 %17, i32* %7
store i32 4195527, i32* %3
store i32 %17, i32* %6
store i64 %17, i64* %4
store i64 4195527, i64* %2
store i64 %17, i64* %5
store <4 x float> %25, <4 x float>* %9
store <4 x float> -43.2, <4 x float>* %12
store <8 x float> %25, <8 x float>* %10
store <8 x float> -43.2, <8 x float>* %13
store <16 x float> %25, <16 x float>* %8
store <16 x float> -43.2, <16 x float>* %11
br i1 %29, label %32, label %30
; <label>:30:
store i32 4195527, i32* %3
store i64 4195527, i64* %2
; <label>:31:
store i32 0, i32* %21, align 1
store i32 4195571, i32* %3
store i64 %17, i64* %4
store i64 4195571, i64* %2
br label %42
; <label>:32:
%33 = load double, double* %19, align 1
%34 = load double, double* %20, align 1
%35 = fcmp ueq double %34, %33
%36 = fcmp ult double %34, %33
%37 = load i32, i32* %14
%38 = or i1 %36, %35
store i32 %37, i32* %14
store i32 4195559, i32* %3
store i64 %17, i64* %4
store i64 4195559, i64* %2
store <4 x float> %33, <4 x float>* %9
store <4 x float> %34, <4 x float>* %12
store <8 x float> %33, <8 x float>* %10
store <8 x float> %34, <8 x float>* %13
store <16 x float> %33, <16 x float>* %8
store <16 x float> %34, <16 x float>* %11
br i1 %38, label %40, label %39
; <label>:39:
store i32 -1, i32* %21, align 1
store i32 4195566, i32* %3
store i64 %17, i64* %4
store i64 4195566, i64* %2
br label %41
; <label>:40:
store i32 1, i32* %21, align 1
store i32 4195566, i32* %3
store i64 %17, i64* %4
store i64 4195566, i64* %2
br label %41
; <label>:41:
store i32 4195571, i32* %3
store i64 4195571, i64* %2
br label %42
; <label>:42:
store i32 %21, i32* %22, align 1
%43 = load i32, i32* %14
store i32 %43, i32* %14
store i32 %21, i32* %16
store i32 4195616, i32* %3
store i64 %21, i64* %15
store i64 %17, i64* %4
store i64 4195616, i64* %2
; <label>:44:
store i32 4195596, i32* %3
store i64 4195596, i64* %2
br label %45
; <label>:45:
%46 = load i32, i32* %14
store i32 %46, i32* %14
store i32 4195628, i32* %3
store i64 %17, i64* %4
store i64 4195628, i64* %2
; <label>:47:
store i32 4195640, i32* %3
store i64 4195640, i64* %2
br label %50
; <label>:48:
store i32 -1, i32* %23, align 1
store i32 4195647, i32* %3
store i64 %17, i64* %4
store i64 4195647, i64* %2
br label %51
; <label>:49:
store i32 1, i32* %23, align 1
store i32 4195647, i32* %3
store i64 %17, i64* %4
store i64 4195647, i64* %2
br label %51
; <label>:50:
store i32 0, i32* %23, align 1
store i32 4195647, i32* %3
store i64 %17, i64* %4
store i64 4195647, i64* %2
br label %51
; <label>:51:
store i32 %23, i32* %16
store i32 %17, i32* %7
store i32 %5, i32* %3
store i64 %23, i64* %15
store i64 %17, i64* %4
store i64 %5, i64* %2
br label %52
; <label>:52:
ret void
}

