#include <stdio.h>

int main(){
  int a;
  char b[32];

  printf("Enter either 0 or 1: ");
  scanf("%d", &a);

  puts("Enter either 'zero' or 'one': ");
  gets(b);

  return 0;
}
