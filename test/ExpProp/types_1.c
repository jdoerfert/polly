void f(int *A, float *B) {
  for (int i = 0; i < 100; i++) {
    int Ai = A[i];
    // split
    B[i] = Ai + 3.0;
  }
}
