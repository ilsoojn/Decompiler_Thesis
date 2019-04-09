define void @fn_400480(%regset* noalias nocapture) {
entry_fn_400480:
int %b;
int %c;
double %d;
double %e;
double %f;
br label %bb_400480
%b = 0;
%c = 10;
%d = 313.24;
%e = 10.0;
%f = %c * %e; + %d;;
br label %exit_fn_400480
return void;
}

