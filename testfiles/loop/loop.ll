; ModuleID = 'testfiles/loop/loop.c'
source_filename = "testfiles/loop/loop.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca double, align 8
  %5 = alloca double, align 8
  %6 = alloca double, align 8
  store i32 0, i32* %1, align 4
  store i32 5, i32* %2, align 4
  store i32 625, i32* %3, align 4
  store double 5.500000e+00, double* %4, align 8
  store double 1.200000e+00, double* %5, align 8
  br label %7

; <label>:7:                                      ; preds = %11, %0
  %8 = load i32, i32* %2, align 4
  %9 = load i32, i32* %3, align 4
  %10 = icmp slt i32 %8, %9
  br i1 %10, label %11, label %14

; <label>:11:                                     ; preds = %7
  %12 = load i32, i32* %2, align 4
  %13 = mul nsw i32 %12, 5
  store i32 %13, i32* %2, align 4
  br label %7

; <label>:14:                                     ; preds = %7
  br label %15

; <label>:15:                                     ; preds = %19, %14
  %16 = load double, double* %4, align 8
  %17 = load double, double* %5, align 8
  %18 = fcmp oge double %16, %17
  br i1 %18, label %19, label %26

; <label>:19:                                     ; preds = %15
  %20 = load double, double* %4, align 8
  %21 = load double, double* %5, align 8
  %22 = fdiv double %20, %21
  store double %22, double* %6, align 8
  %23 = load double, double* %6, align 8
  %24 = load double, double* %5, align 8
  %25 = fadd double %24, %23
  store double %25, double* %5, align 8
  br label %15

; <label>:26:                                     ; preds = %15
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}
