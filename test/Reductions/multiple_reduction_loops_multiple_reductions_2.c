void f(int *__restrict__ S, int *__restrict__ B, int *__restrict__ sum) {
  int i, j, k;
  for (i = 0; i < 100; i++) {
    for (j = 0; j < 100; j++) {
      for (k = 0; k < 100; k++) {
        S[j] += B[i + j + k];
        sum[i] += B[i];
      }
    }
  }
}

