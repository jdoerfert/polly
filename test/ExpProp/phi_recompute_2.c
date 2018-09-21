    void f(int *restrict B, int A[100]) {
      int N = 100;

      for (int i = 0; i < N; i++)
        A[i] = i+1;
      for (int i = 0; i < N; i++)
        B[i] = A[i] * 2;
    }
