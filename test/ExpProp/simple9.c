#include<stdlib.h>

void f(int *A) {
  int *tmp = malloc(sizeof(int) * 100);

  for (int i = 0; i < 100; i++)
  S0:
    A[i] = i;

  for (int j = 0; j < 10; j++) {

    for (int i = 0; i < 100; i++)
    S1:
      tmp[i] = A[i];

    for (int i = 0; i < 100; i++)
    S2:
      tmp[i] += A[i] + i;

    for (int i = 0; i < 100; i++)
    S3:
      tmp[i] += i;

    for (int i = 0; i < 100; i++)
    S4:
      A[i] += tmp[i];
  }

  free(tmp);
}
