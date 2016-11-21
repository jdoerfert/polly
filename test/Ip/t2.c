
void f(int *A, int i, int k) {
	for(int j = 0; j < k; j++)
		A[j + i] = i;
}

int *G;
void loop(int N) {
	for (int i = 0; i < 200; i++){
    for(int j = 0; j < 100; j++)
		  f(G, i, j);
  }
}

