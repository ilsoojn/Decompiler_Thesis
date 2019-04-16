void main ( %regset* noalias nocapture ) {
	int %d
	double %c
	double %b
	%b = 1.1
	%c = 121.121
	%d = 0
	%ZF_0 = %b == %c
	%CF_0 = %b < %c
	%145 = %CF_0 | %ZF_0
	%CC_A_0 = %145 ^ true
	if (%145) {
	} else {
		%69 = %b / %c
		%c = %69
	}
	return void;
}

