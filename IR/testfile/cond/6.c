// condition SIX

int main(){

  char c = 'c', d; // ascii decimal 96

  switch(c){
    case '0':
      d = '0';
      break;

    case 'A':
      d = 'A';
      break;
    case 'a':
      d = 'a';
      break;

    default:
      d = '-';
  }
  return 0;
}
