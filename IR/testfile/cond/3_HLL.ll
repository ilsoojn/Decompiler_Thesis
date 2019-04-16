void main ( %regset* noalias nocapture ) {
	i8 %e
	int %d
	double %c
	double %b
	%b = -12.3
	%c = 32.1
	%d = 32
	%e = 97
	%PF_0 = false
	if (true) {
		%ZF_01 = %b == %c
		%CC_NE_0 = %ZF_01 ^ true
		if (%ZF_01) {
		} else {
			%ZF_04 = %b == %c
			%CF_06 = %b < %c
			%CC_BE_0 = %CF_06 | %ZF_04
			if (%CF_06 | %ZF_04) {
				%e = 48
			}
		}
	}
	return void;
}

