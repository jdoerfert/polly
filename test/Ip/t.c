
void f(int *A, int i, int k) {
	for(int j = 0; j < A[k]; j++)
		A[j + i] = i;
}

int *G;
void loop(int N) {
	for (int i = 0; i < N; i++)
		f(G, i, i);
}

