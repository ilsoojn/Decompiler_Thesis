; ModuleID = 'testfiles/conditional/cond.c'
source_filename = "testfiles/conditional/cond.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca double, align 8
  %3 = alloca double, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  store double -4.320000e+01, double* %2, align 8
  store double -3.210000e+01, double* %3, align 8
  %6 = load double, double* %2, align 8
  %7 = load double, double* %3, align 8
  %8 = fcmp oeq double %6, %7
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %0
  store i32 0, i32* %4, align 4
  br label %17

; <label>:10:                                     ; preds = %0
  %11 = load double, double* %2, align 8
  %12 = load double, double* %3, align 8
  %13 = fcmp olt double %11, %12
  br i1 %13, label %14, label %15

; <label>:14:                                     ; preds = %10
  store i32 -1, i32* %4, align 4
  br label %16

; <label>:15:                                     ; preds = %10
  store i32 1, i32* %4, align 4
  br label %16

; <label>:16:                                     ; preds = %15, %14
  br label %17

; <label>:17:                                     ; preds = %16, %9
  %18 = load i32, i32* %4, align 4
  switch i32 %18, label %21 [
    i32 -1, label %19
    i32 1, label %20
  ]

; <label>:19:                                     ; preds = %17
  store i32 -1, i32* %5, align 4
  br label %22

; <label>:20:                                     ; preds = %17
  store i32 1, i32* %5, align 4
  br label %22

; <label>:21:                                     ; preds = %17
  store i32 0, i32* %5, align 4
  br label %22

; <label>:22:                                     ; preds = %21, %20, %19
  %23 = load i32, i32* %5, align 4
  ret i32 %23
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}
