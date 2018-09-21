#include <stdio.h>

int A[100], B[100], C[100], D[100];

void f(int N) {
  for (int i = 1; i < N - 3; i++) {
    B[i] = A[i] + 1;
    C[i] = B[i] + 2;
    D[i] = C[i] + C[i-1];
  }
}

void eval(int N) {
  f(N);
  int a = 0, b = 0, c = 0, d = 0;
  for (int i = 0; i < 100; i++) {
    a += A[i];
    b += B[i];
    c += C[i];
    d += D[i];
  }
  printf("[eval f(%i)], s = %i,%i,%i,%i\n", N, a,b,c,d);
}

int main() {
  for (int i = 0; i < 100; i++) {
    A[i] = 1 * i;
    B[i] = 2 * i;
    C[i] = 3 * i;
    D[i] = 4 * i;
  }

  eval(1);
  eval(2);
  eval(3);
  eval(4);
  eval(5);
  eval(47);
  eval(100);
  return 0;
}
