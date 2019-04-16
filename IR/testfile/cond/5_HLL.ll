void main ( %regset* noalias nocapture ) {
	i8 %c
	i8 %b
	%b = 97
	%CC_E_0 = %b == 65
	if (%b == 65) {
		%c = 65
	} else {
		%CC_E_09 = %b == 97
		if (%b == 97) {
			%c = 97
		} else {
			%CC_NE_025 = %b != 48
			if (%b != 48) {
				%c = 45
			} else {
				%c = 48
			}
		}
	}
	return void;
}

