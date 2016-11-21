int Q[2];
int ZERO;
int ONE;

void system(void);

int h() {
  if (ZERO)
    system();

  return Q[1];
}

int g() {
  if (ONE != 1)
    system();
  return h() + Q[0] + Q[h()];
}

void f(int *A) {
  for (int i = 0; i < 1000; i++)
    for (int j = 0; j < 1000; j++)
      A[i+j +ZERO] = g();
}
