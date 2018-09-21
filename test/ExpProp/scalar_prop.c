void f(double *A, double *B) {
  double x;
  for (int i = 0; i < 100; i++) {
    x = 3.3 * B[i];
    // split
    A[i] = x;
  }

  for (int i = 0; i < 100; i++) {
    A[i] = x;
  }
}
