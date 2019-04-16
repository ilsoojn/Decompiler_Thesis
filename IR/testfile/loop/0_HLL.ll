void main ( %regset* noalias nocapture ) {
	int %d
	int %c
	int %b
	%b = 5
	%c = 625
	%d = 0
	%CC_LE_0 = %d <= 4
	if (%d <= 4) {
		%b = %b
	}
	return void;
}

