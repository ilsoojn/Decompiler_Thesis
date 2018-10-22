#include <stdio.h>

int main () {

	int x = 5;		// %RSP-16
	double y = 12345.4;	// %RSP-24
	int condition  = 100; // %RSP-28

	int i;
	printf("\nSTART\nt: 0\tcc: %d\ty: %fl\n", condition, y);
	for(i = 0; i < x; i++){

		double tmp = y / x;

		if (tmp < condition){
			y += tmp;
			printf("t: %fl\tcc: %d\ty: %fl\n", tmp, condition, y);
		}else{
			condition += tmp;
			y -= (int) tmp;
			printf("t: %fl\tcc: %d\ty: %fl\n", tmp, condition, y);
		}
	}
	printf("\nEND");
	return 0;
}
