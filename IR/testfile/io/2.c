// InOut TWO

#include <stdio.h>

int main(){

  char buff[32];
  scanf("%s\b", buff);
  fgets(buff, 32, stdin);
}
