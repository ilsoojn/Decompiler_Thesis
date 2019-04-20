void main ( %regset* noalias nocapture ) {
	int %d
	int %c
	short %b
	 %b = 5
	 %c = 123
%d = %c * %b - %c + %b
return void
}

