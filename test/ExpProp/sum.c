#include <stdio.h>

void f(double *d) {
  if (d) {
    double s = 1.0;
    for (int i = 0; i < 100; i++)
      s += 2.0 * i;
    *d = s;
  }
}

int main(int argc, char *argv[]) {
  double d;
  f(&d);
  printf("f() = %lf\n", d);
  return 0;
}
