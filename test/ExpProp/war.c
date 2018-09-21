void f(int *A, int *B, int *C) {
  for (int i = 0; i < 100; i++)
S0:    A[i] = C[i];
  for (int i = 0; i < 100; i++)
S1:    B[i] += C[i];
  for (int i = 0; i < 100; i++)
S2:    C[i] = 0;
  for (int i = 0; i < 100; i++)
S3:    B[i] += A[i];
}
