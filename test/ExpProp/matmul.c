void f(double *C, double *D, double *E, int L) {
  for (int i = 0; i < L; i++)
    for (int j = 0; j < L; j++) {
      double sum = 0;
      for (int k = 0; k < L; k++)
        sum += C[L * i + k] * D[L * k + j];
      E[L * i + j] = sum;
    }
}
