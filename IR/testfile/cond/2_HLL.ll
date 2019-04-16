void main ( %regset* noalias nocapture ) {
	int %d
	double %c
	double %b
	%b = 12.3
	%c = 12.3
	%d = 0
	%PF_0 = false
	if (true) {
		%ZF_01 = %b == %c
		%CC_NE_0 = %ZF_01 ^ true
		if (%ZF_01) {
		}
	}
	%d = -1
	%d = 1
	return void;
}

