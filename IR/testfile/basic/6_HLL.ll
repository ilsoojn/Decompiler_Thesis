void main ( %regset* noalias nocapture ) {
	double %c
	float %b
	%b = -11.109999656677246
	%c = 12345.678
	%65 = %b + %c
	%90 = %65 / %c
	%XMM1_2 = %90
	%c = %XMM1_2
	return void;
}

