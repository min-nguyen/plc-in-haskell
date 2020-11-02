// micro-C example 6 -- return a result via a pointer argument; nested blocks

void main(int n) {
  int i;
  i = 0;
  while (i < n) {
    int n;
    fac(i, &n);
    print n;
    i = i + 1;
  }
  print n;
}

void fac(int n, int *res) {
  if (n == 0)
    *res = 1;
  else {
    int tmp;
    fac(n-1, &tmp);
    *res = tmp * n;
  }
}
