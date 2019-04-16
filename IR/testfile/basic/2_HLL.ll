void main ( %regset* noalias nocapture ) {
	long %d
	long %c
	short %b
	%b = 5
	%c = 123
	%RAX_0 = %c
	%RDX_0 = %RAX_0 call 63
	%29 = %RDX_0 << 64
	%30 = %29 | %c
	%32 = %30 / %b
	%d = %32
	return void;
}

