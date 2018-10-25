int main(){

int a = 5, b = 625;
double x = 5.5, y = 1.2;

for(; a < b;){
	a = a * 5;
}

while(x >= y){
	double tmp = x / y;
	y += tmp;
}

return 0;
}
