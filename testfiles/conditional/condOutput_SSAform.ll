define void @fn_400480(%regset* noalias nocapture) {
; <label>:1:
%2 = alloca i64
%3 = alloca i64, align 8
%4 = alloca i32, align 4
%5 = alloca double, align 8
%6 = alloca double, align 8
%7 = alloca i32, align 4
%8 = alloca i32, align 4
%9 = alloca i32, align 4
%10 = alloca i32, align 4
%11 = alloca i32, align 4
%12 = alloca i32, align 4
br label %13
; <label>:13:
store i64 %2, i64* %3, align 1
store i32 0, i32* %4, align 1
store double -43.2, double* %5, align 1
store double -32.1, double* %6, align 1
%14 = load double, double* %5, align 1
%15 = load double, double* %6, align 1
%16 = fcmp ueq double %14, %15
%17 = fcmp uno double %14, %15
%18 = fcmp ult double %14, %15
%19 = load i32, i32* %7
%20 = shl i32 %18, 0
%21 = or i32 %20, %19
%22 = shl i32 %17, 2
%23 = or i32 %22, %21
%24 = shl i32 false, 4
%25 = or i32 %24, %23
%26 = shl i32 %16, 6
%27 = or i32 %26, %25
%28 = shl i32 false, 7
%29 = or i32 %28, %27
%30 = shl i32 false, 11
%31 = or i32 %29, %30
%32 = xor i1 %16, true
store i32 %19, i32* %7
br i1 %32, label %36, label %33 ;
; <label>:33:
%34 = lshr i32 %31, 2
br i1 %34, label %36, label %35 ; x != y
; <label>:35:
store i32 0, i32* %8, align 1  ; x != y False : If(x == y True)
br label %46
; <label>:36:
%37 = load double, double* %5, align 1
%38 = load double, double* %6, align 1
%39 = fcmp ueq double %38, %37
%40 = fcmp ult double %38, %37
%41 = load i32, i32* %7
%42 = or i1 %40, %39
store i32 %41, i32* %7
br i1 %42, label %44, label %43 ; x >= y
; <label>:43:
store i32 -1, i32* %8, align 1 ; x >= y False : Else If(x < y True)
br label %45
; <label>:44:
store i32 1, i32* %8, align 1 ; x >= y True : Else
br label %45
; <label>:45:
br label %46
; <label>:46:
store i32 %8, i32* %9, align 1
store i32 %RSP-35, i32* %10, align 1
%47 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %8, i32 -1)
%48 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %8, i32 -1)
%49 = call i8 @llvm.ctpop.i8(i8 %RSP-35)
%50 = icmp eq i1 %49, false
%51 = load i32, i32* %7
%52 = shl i32 %48, 0
%53 = or i32 %52, %51
%54 = shl i32 %50, 2
%55 = or i32 %54, %53
%56 = shl i32 false, 4
%57 = or i32 %56, %55
%58 = shl i32 %ZF_05, 6
%59 = or i32 %58, %57
%60 = shl i32 %SF_0, 7
%61 = or i32 %60, %59
%62 = shl i32 %47, 11
%63 = or i32 %61, %62
%64 = lshr i32 %63, 6
store i32 %51, i32* %7
br i1 %64, label %86, label %65
; <label>:65:
br label %66
; <label>:66:
store i32 %RSP-45, i32* %11, align 1
%67 = call { i32, i1 } @llvm.ssub.with.overflow.i32(i32 %9, i32 1)
%68 = call { i32, i1 } @llvm.usub.with.overflow.i32(i32 %9, i32 1)
%69 = call i8 @llvm.ctpop.i8(i8 %RSP-45)
%70 = icmp eq i1 %69, false
%71 = load i32, i32* %7
%72 = shl i32 %68, 0
%73 = or i32 %72, %71
%74 = shl i32 %70, 2
%75 = or i32 %74, %73
%76 = shl i32 false, 4
%77 = or i32 %76, %75
%78 = shl i32 %ZF_08, 6
%79 = or i32 %78, %77
%80 = shl i32 %SF_09, 7
%81 = or i32 %80, %79
%82 = shl i32 %67, 11
%83 = or i32 %81, %82
%84 = lshr i32 %83, 6
store i32 %71, i32* %7
br i1 %84, label %87, label %85
; <label>:85:
br label %88
; <label>:86:
store i32 -1, i32* %12, align 1
br label %89
; <label>:87:
store i32 1, i32* %12, align 1
br label %89
; <label>:88:
store i32 0, i32* %12, align 1
br label %89
; <label>:89:
br label %90
; <label>:90:
ret void
}
