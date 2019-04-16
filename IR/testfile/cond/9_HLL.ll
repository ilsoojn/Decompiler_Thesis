void main ( %regset* noalias nocapture ) {
	int %e
	int %d
	double %c
	double %b
	%b = -12.3
	%c = 32.1
	%PF_0 = false
	if (true) {
		%ZF_01 = %b == %c
		%CC_NE_0 = %ZF_01 ^ true
		if (%ZF_01) {
			%d = 0
			%CC_E_0 = %d == -1
			%e = -1
			%CC_E_020 = %d == 1
			%e = 1
			%e = 0
		} else {
			%ZF_04 = %b == %c
			%CF_06 = %b < %c
			%CC_BE_0 = %CF_06 | %ZF_04
			if (%CF_06 | %ZF_04) {
				%d = 1
			} else {
				%d = -1
			}
		}
	}
	return void;
}

