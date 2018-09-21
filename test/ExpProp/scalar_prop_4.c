void f(double *A) {
  for (int i = 0; i < 100; i++) {
    double x = 3.3 * A[i];
    // split
    A[i] = x;
  }
}
