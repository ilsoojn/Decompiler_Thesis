void main ( %regset* noalias nocapture ) {
	int %c
	int %b
	%b = 0
	%c = 10
	%CC_E_0 = %b == -1
	if (%b == -1) {
		%c = -1
	} else {
		%CC_E_09 = %b == 1
		if (%b == 1) {
			%c = 1
		} else {
			%c = 0
		}
	}
	return void;
}

