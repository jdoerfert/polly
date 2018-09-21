    void f(int *restrict B, int A[100]) {
      int N = 100;

      for (int i = 0; i < N; i++)
        A[i] = 5*i;
      for (int i = 3; i < N; i++)
        B[i] = A[i-3] * 2;
    }
