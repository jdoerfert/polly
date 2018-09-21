void f(int *A) {
  for (int i = 0; i < 100; i++)
  for (int h = 0; h < 100; h++) {
    int x = i + h;
    // split
    A[i] += x;
  }
}
