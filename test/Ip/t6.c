int system(void);

int C;

int repr0(int *A, int N, int C) {
R00:
  if (C)
R01:
    return 0;

R02:
  for (int i = 0; i < N; i++)
R03:
    A[i]++;

R04:
  return system();
}

int repr1(int *A, int repr1N, int C) {
R10:
  if (C)
R11:
    return repr1N + A[-1];

R12:
  system();

  int s = 0;
R13:
  for (int i = 0; i < repr1N; i++)
R14:
    s += A[i]++;

  return s;
}

int* repr2(int *A, int N, int repr2C) {
R20:
  if (repr2C)
R21:
    return &A[-47];

R22:
  A[5]++;
  return 0;
}

void inline0(int *A, int N, int C) {
I00:
  for (int i = 0; i < N; i++)
I01:
    A[i]++;
}

void inline1(int *A, int N, int C) {
I10:
  if (C)
I11:
    return;

I12:
  for (int i = 0; i < N; i++)
I13:
    A[i]++;
}

void inline2(int *A, int N, int C) {
I20:
  if (C)
I21:
    system();

I22:
  for (int i = 0; i < N; i++)
I23:
    A[i]++;
}

void test(int *A, int N) {
  int CL = C;

  repr0(A, N, C);
  inline0(A, N, 0);
  int Repr0Res = repr0(A, N, CL);
  int Repr1Res = repr1(A, Repr0Res, C);
  inline1(A, Repr1Res, Repr0Res);
  int* Repr2Res = repr2(A, N, C);
  inline2(Repr2Res, N, CL);
}
