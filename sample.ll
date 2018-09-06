; ModuleID = 'sample.c'
source_filename = "sample.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.str = private unnamed_addr constant [27 x i8] c"\0ASTART\0At: 0\09cc: %d\09y: %fl\0A\00", align 1
@.str.1 = private unnamed_addr constant [22 x i8] c"t: %fl\09cc: %d\09y: %fl\0A\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"\0AEND\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca double, align 8
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca double, align 8
  store i32 0, i32* %1, align 4
  store i32 5, i32* %2, align 4
  store double 1.234540e+04, double* %3, align 8
  store i32 100, i32* %4, align 4
  %7 = load i32, i32* %4, align 4
  %8 = load double, double* %3, align 8
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str, i32 0, i32 0), i32 %7, double %8)
  store i32 0, i32* %5, align 4
  br label %10

; <label>:10:                                     ; preds = %47, %0
  %11 = load i32, i32* %5, align 4
  %12 = load i32, i32* %2, align 4
  %13 = icmp slt i32 %11, %12
  br i1 %13, label %14, label %50

; <label>:14:                                     ; preds = %10
  %15 = load double, double* %3, align 8
  %16 = load i32, i32* %2, align 4
  %17 = sitofp i32 %16 to double
  %18 = fdiv double %15, %17
  store double %18, double* %6, align 8
  %19 = load double, double* %6, align 8
  %20 = load i32, i32* %4, align 4
  %21 = sitofp i32 %20 to double
  %22 = fcmp olt double %19, %21
  br i1 %22, label %23, label %31

; <label>:23:                                     ; preds = %14
  %24 = load double, double* %6, align 8
  %25 = load double, double* %3, align 8
  %26 = fadd double %25, %24
  store double %26, double* %3, align 8
  %27 = load double, double* %6, align 8
  %28 = load i32, i32* %4, align 4
  %29 = load double, double* %3, align 8
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str.1, i32 0, i32 0), double %27, i32 %28, double %29)
  br label %46

; <label>:31:                                     ; preds = %14
  %32 = load double, double* %6, align 8
  %33 = load i32, i32* %4, align 4
  %34 = sitofp i32 %33 to double
  %35 = fadd double %34, %32
  %36 = fptosi double %35 to i32
  store i32 %36, i32* %4, align 4
  %37 = load double, double* %6, align 8
  %38 = fptosi double %37 to i32
  %39 = sitofp i32 %38 to double
  %40 = load double, double* %3, align 8
  %41 = fsub double %40, %39
  store double %41, double* %3, align 8
  %42 = load double, double* %6, align 8
  %43 = load i32, i32* %4, align 4
  %44 = load double, double* %3, align 8
  %45 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str.1, i32 0, i32 0), double %42, i32 %43, double %44)
  br label %46

; <label>:46:                                     ; preds = %31, %23
  br label %47

; <label>:47:                                     ; preds = %46
  %48 = load i32, i32* %5, align 4
  %49 = add nsw i32 %48, 1
  store i32 %49, i32* %5, align 4
  br label %10

; <label>:50:                                     ; preds = %10
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0))
  ret i32 0
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}
