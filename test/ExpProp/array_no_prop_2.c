void f(int *tmp, int *y, int NX, int NY) {
    int i = 0;
A:  tmp[i] = 0;
    for (int j = 0; j < NY; j++)
      tmp[i] = tmp[i] + 3;
    for (int j = 0; j < NY; j++)
C:    y[j] = y[j] * tmp[i];
}
