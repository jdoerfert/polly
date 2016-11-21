int f2(int *A, int i, int k) {
	for(int j = 0; j < k; j++)
		A[j] = i * 2;
  return 0;
}


int f(int *A, int i, int k) {
	for(int j = 0; j < k; j++)
		A[j + i] = i;
  f2(A, 10, k);
  return 0;
}

int *G;
void loop(int N) {
	for (int i = 0; i < 200; i++){
    for(int j = 0; j < 100; j++)
		  f(G, i, j);
    for(int k = 100; k < 200; k++)
      G[k] = 0;
  }
}

