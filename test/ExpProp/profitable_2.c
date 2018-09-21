    void f(int *A) {
      for (int i = 0; i < 100; i++) {
        int y;
        for (int h = 0; h < 100; h++) {
          int x = i + h % 5;
          // split
          A[i] += x;
          y = x % 3;
        }
       A[i] += y;
    }
    }
