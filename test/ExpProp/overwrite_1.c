void f(int *A, int *B, int *C) {
  for (int i = 0; i < 100; i++) {
    for (int j = 0; j < 100; j++)
      A[i] = A[i] + B[j];
    C[i] = A[i];
  }
}
