void main ( %regset* noalias nocapture ) {
	int %c
	i8 %b
	%b = 97
	%CC_G_0 = %b > 121
	if (%b > 121) {
		%c = -1
	} else {
		%c = 1
	}
	return void;
}

