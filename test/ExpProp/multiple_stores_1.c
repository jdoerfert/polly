void f(int *A, int *B) {
  for (int i = 0; i < 100; i++) {
    A[i] = 0;
    A[i] = A[i] + i;
  }
  for (int i = 0; i < 100; i++) {
    B[i] += A[i];
  }
}
