// InOut FOUR

#include <stdio.h>

int main(){
  char buff[32];
  char *ptr = buff;
  scanf("%s\n", ptr);
  fgets(ptr, 32, stdin);
}
