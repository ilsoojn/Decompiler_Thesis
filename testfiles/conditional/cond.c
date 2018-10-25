int main(){

	double x = -43.2, y = -32.1;
	int a, b;

	if(x == y){ a = 0;
	}else if(x < y){ a = -1;
	}else{ a = 1; }

	switch(a){
		case -1:
			b = -1;
			break;
		case 1:
			b = 1;
			break;
		default:
			b = 0;
	}
	return b;
}
