int Glob[2];

void system(void);

int h(void) {
  if (Glob[0])
    system();
  return Glob[1];
}

int g(int i) {
  if (Glob[i])
    system();
  return Glob[h()];
}

void f(int *A, int N) {
  for (int i = 0; i < N ; i++)
    for (int j = 0; j < i; j++)
      A[i+j] = g(0) + h();
}
