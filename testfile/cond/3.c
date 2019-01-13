// condition THREE

int main(){

  double x = -12.3, y = 32.1;
  int a = 32;
  char c = 'a'; // ascii decimal 96

  if (x == y){
    c = c - a;
  }else if (x < y){
    c++;
  }else{
    c = '0';
  }

  return 0;
}
