void main ( %regset* noalias nocapture ) {
	i8 %c
	int %b
	%b = 5
	%c = 122
	%CC_G_0 = %c > 99
	if (%c > 99) {
		%c = 45
	} else {
		%49 = %b - 1
		%b = %49
		%CC_G_07 = %b > 0
		if (%b > 0) {
		}
	}
	return void;
}

