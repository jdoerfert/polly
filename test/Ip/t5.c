void system();

int C;

void repr0(int *A, int N, int C) {
R00:
  if (C)
R01:
    return;

R02:
  for (int i = 0; i < N; i++)
R03:
    A[i]++;

R04:
  system();
}

void repr1(int *A, int N, int C) {
R10:
  if (C)
R11:
    return;

R12:
  system();

R13:
  for (int i = 0; i < N; i++)
R14:
    A[i]++;

}

void repr2(int *A, int N, int C) {
R20:
  if (C)
R21:
    return;

R22:
  A[5]++;
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
  repr0(A, N, CL);
  repr1(A, N, C);
  inline1(A, N, C);
  repr1(A, N, CL);
  repr2(A, N, C);
  inline2(A, N, CL);
  repr2(A, N, CL);
}
