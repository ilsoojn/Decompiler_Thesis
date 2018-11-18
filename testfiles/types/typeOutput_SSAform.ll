define void @fn_400480(%regset* noalias nocapture) {
; <label>:1:
%2 = alloca i64
%3 = alloca i64, align 8
%4 = alloca i32, align 4
%5 = alloca i16, align 2
%6 = alloca i16, align 2
%7 = alloca i32, align 4
%8 = alloca i32, align 4
%9 = alloca i64, align 8
%10 = alloca i64, align 8
%11 = alloca i64, align 8
%12 = alloca i64, align 8
%13 = alloca i8, align 1
%14 = alloca i8, align 1
%15 = alloca float, align 4
%16 = alloca double, align 8
%17 = alloca i64, align 8
%18 = alloca i64, align 8
%19 = alloca i32, align 4
br label %20
; <label>:20:
store i64 %2, i64* %3, align 1
store i32 0, i32* %4, align 1
store i16 -32100, i16* %5, align 1
store i16 -104, i16* %6, align 1
store i32 -4967296, i32* %7, align 1
store i32 -2123123123, i32* %8, align 1
store i64 -2111111111, i64* %9, align 1
store i64 4200000000, i64* %10, align 1
store i64 -123123123, i64* %11, align 1
store i64 123123123123, i64* %12, align 1
store i8 32, i8* %13, align 1
store i8 33, i8* %14, align 1
store float 1234.56787109375, float* %15, align 1
store double 9876.54321, double* %16, align 1
store i64 %8, i64* %17, align 1
store i64 %16, i64* %18, align 1
%21 = load i32, i32* %19
store i32 %21, i32* %19
br label %22
; <label>:22:
ret void
}

