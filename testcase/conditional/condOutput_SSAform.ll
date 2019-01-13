define void @fn_400480(%regset* noalias nocapture) {
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
%15 = alloca i32
%16 = alloca i64
%17 = alloca i32
%18 = alloca i64, align 8
%19 = alloca i32, align 4
%20 = alloca double, align 8
%21 = alloca double, align 8
%22 = alloca i32, align 4
%23 = alloca i32, align 4
%24 = alloca i32, align 4
br label %25
; <label>:25:
store i64 %4, i64* %18, align 1
store i32 0, i32* %19, align 1
store double -43.2, double* %20, align 1
store double -32.1, double* %21, align 1
%26 = load double, double* %20, align 1
%27 = load double, double* %21, align 1
%28 = fcmp ueq double %26, %27
%29 = fcmp uno double %26, %27
%30 = fcmp ult double %26, %27
%31 = load i32, i32* %14
%32 = %CF_0.%PF_0.false.%28.false.false
%33 = xor i1 %28, true
store i32 %31, i32* %14
store i32 %18, i32* %7
store i32 %32, i32* %15
store i32 4195527, i32* %3
store i32 %18, i32* %6
store i64 %18, i64* %4
store i64 4195527, i64* %2
store i64 %18, i64* %5
store <4 x float> %26, <4 x float>* %9
store <4 x float> -43.2, <4 x float>* %12
store <8 x float> %26, <8 x float>* %10
store <8 x float> -43.2, <8 x float>* %13
store <16 x float> %26, <16 x float>* %8
store <16 x float> -43.2, <16 x float>* %11
br i1 %33, label %37, label %34
; <label>:34:
%35 = lshr i32 %32, 2
store i32 %32, i32* %15
store i32 4195527, i32* %3
store i64 4195527, i64* %2
br i1 %35, label %37, label %36
; <label>:36:
store i32 0, i32* %22, align 1
store i32 4195571, i32* %3
store i64 %18, i64* %4
store i64 4195571, i64* %2
br label %49
; <label>:37:
%38 = load double, double* %20, align 1
%39 = load double, double* %21, align 1
%40 = fcmp ueq double %39, %38
%41 = fcmp uno double %39, %38
%42 = fcmp ult double %39, %38
%43 = load i32, i32* %14
%44 = %CF_04.%PF_03.false.%40.false.false
%45 = or i1 %42, %40
store i32 %43, i32* %14
store i32 %44, i32* %15
store i32 4195559, i32* %3
store i64 %18, i64* %4
store i64 4195559, i64* %2
store <4 x float> %38, <4 x float>* %9
store <4 x float> %39, <4 x float>* %12
store <8 x float> %38, <8 x float>* %10
store <8 x float> %39, <8 x float>* %13
store <16 x float> %38, <16 x float>* %8
store <16 x float> %39, <16 x float>* %11
br i1 %45, label %47, label %46
; <label>:46:
store i32 -1, i32* %22, align 1
store i32 4195566, i32* %3
store i64 %18, i64* %4
store i64 4195566, i64* %2
br label %48
; <label>:47:
store i32 1, i32* %22, align 1
store i32 4195566, i32* %3
store i64 %18, i64* %4
store i64 4195566, i64* %2
br label %48
; <label>:48:
store i32 4195571, i32* %3
store i64 4195571, i64* %2
br label %49
; <label>:49:
store i32 %22, i32* %23, align 1
%50 = load i32, i32* %14
store i32 %50, i32* %14
store i32 %22, i32* %17
store i32 4195616, i32* %3
store i64 %22, i64* %16
store i64 %18, i64* %4
store i64 4195616, i64* %2
; <label>:51:
store i32 4195596, i32* %3
store i64 4195596, i64* %2
br label %52
; <label>:52:
%53 = load i32, i32* %14
store i32 %53, i32* %14
store i32 4195628, i32* %3
store i64 %18, i64* %4
store i64 4195628, i64* %2
; <label>:54:
store i32 4195640, i32* %3
store i64 4195640, i64* %2
br label %57
; <label>:55:
store i32 -1, i32* %24, align 1
store i32 4195647, i32* %3
store i64 %18, i64* %4
store i64 4195647, i64* %2
br label %58
; <label>:56:
store i32 1, i32* %24, align 1
store i32 4195647, i32* %3
store i64 %18, i64* %4
store i64 4195647, i64* %2
br label %58
; <label>:57:
store i32 0, i32* %24, align 1
store i32 4195647, i32* %3
store i64 %18, i64* %4
store i64 4195647, i64* %2
br label %58
; <label>:58:
store i32 %24, i32* %17
store i32 %18, i32* %7
store i32 %5, i32* %3
store i64 %24, i64* %16
store i64 %18, i64* %4
store i64 %5, i64* %2
br label %59
; <label>:59:
ret void
}

