    #include <stdio.h>

    int A[100], B[100], C[100];

    void f(int N) {
      for (int i = 0; i < N; i++) {
        B[i] = i + 1;
        C[i] = i - 1;
        A[i] = B[i] + C[i];
      }
      for (int i = 3; i < N-1; i++) {
        B[i] -= B[i + 1];
        C[i] -= C[i - 1];
        A[i] += B[i-2] + C[i-3];
      }
    }

    void eval(int N) {
      f(N);
      int s = 0;
      for (int i = 0; i < 100; i++)
        s += A[i] * B[i] + C[i];
      printf("[eval f(%i)], s = %i\n", N, s);
    }

    int main() {
      for (int i = 0; i < 100; i++) {
        A[i] = 1 * i;
        B[i] = 2 * i;
        C[i] = 3 * i;
      }

      eval(1);
      eval(2);
      eval(3);
      eval(4);
      eval(5);
      eval(47);
      eval(100);
      return 0;
    }
