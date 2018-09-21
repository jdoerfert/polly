void f(int *A, float *B) {
  for (int i = 0; i < 100; i++)
    A[i] = i + 1;
  for (int i = 0; i < 100; i++)
    B[i] = A[i] + 3.0;
}
