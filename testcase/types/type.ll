; ModuleID = 'type.c'
source_filename = "type.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i16, align 2
  %3 = alloca i16, align 2
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca i64, align 8
  %10 = alloca i8, align 1
  %11 = alloca i8, align 1
  %12 = alloca float, align 4
  %13 = alloca double, align 8
  %14 = alloca i32*, align 8
  %15 = alloca double*, align 8
  store i32 0, i32* %1, align 4
  store i16 -32100, i16* %2, align 2
  store i16 -104, i16* %3, align 2
  store i32 -4967296, i32* %4, align 4
  store i32 -2123123123, i32* %5, align 4
  store i64 -2111111111, i64* %6, align 8
  store i64 4200000000, i64* %7, align 8
  store i64 -123123123, i64* %8, align 8
  store i64 123123123123, i64* %9, align 8
  store i8 32, i8* %10, align 1
  store i8 33, i8* %11, align 1
  store float 0x40934A4580000000, float* %12, align 4
  store double 0x40C34A4587E7C06E, double* %13, align 8
  store i32* %5, i32** %14, align 8
  store double* %13, double** %15, align 8
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}
