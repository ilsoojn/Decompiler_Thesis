void main ( %regset* noalias nocapture ) {
	int %d
	double %c
	double %b
	%b = 1.1
	%c = 121.121
	%d = 0
	%128 = %c - 123.0
	%ZF_0 = %128 == %b
	%CF_0 = %128 < %b
	%158 = %CF_0 | %ZF_0
	%CC_A_0 = %158 ^ true
	if (%158) {
	} else {
		%69 = %b / %c
		%c = %69
	}
	return void;
}

