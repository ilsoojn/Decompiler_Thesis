#include <stdio.h>

int main(){

  int a = 10;
  double b = 20, x = 10;

  double y = a * x + b;

  return 0;
}

--- a ---
%52 = %RSP-16
store i32 10, i32* %52, align 1

--- b ---
%37 = 4195680
%38 = load double, double* %37, align 1
%39 = %38
...
%56 = %RSP-24
store double %39, double* %56, align 1

--- x ---
%24 = 4195672
%25 = load double, double* %24, align 1
%26 = %25
...
%60 = %RSP-8-24
store double %26, double* %60, align 1

--- b ---
%64 = [ %RSP -16 ]
%65 = %64

%75 = %RSP-8-24
%76 = load double, double* %75, align 1

%77 = fmul double %65, %76

%78 = %77
%88 = %RSP-8-16
%89 = load double, double* %88, align 1

%90 = fadd double %78, %89
%91 = %90
%101 = %RSP-8-32
store double %91, double* %101, align 1
