void main ( %regset* noalias nocapture ) {
	int %c
	short %b
	%b = -5
	%c = 123
	%EAX_0 = %c
	%EDX_0 = %EAX_0 call 31
	%29 = %EDX_0 << 32
	%30 = %29 | %c
	%32 = %30 / %b
	%c = %32
	return void;
}

