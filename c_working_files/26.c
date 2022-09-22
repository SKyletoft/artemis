#include <stdio.h>

typedef unsigned long N;
typedef signed long Z;

N g (N b) { return b * 4; }

N h (N a, N b, N c, N d) {return a + b * c - d;}

N i (N a, N b, N c, N d, N e, N f, N g, N h) {return a + b * c - d;}
N l (N a, N b, N c, N d, N e, N f, N g, N h, N i) {return a + b * c - d;}

N j (N x) {return x;}

N k (N x) {
	return x + 5;
}

N f(N a) {
	return	0
		+ g(a)
		+ h(a+ 2, 1, 2, 3)
		+ i(1, 2, 3, 4, 5, 6, 7, 8)
		+ j(8)
		+ k(2)
		+ l(1, 2, 3, 4, 5, 6, 7, 8, 9);
}

int main() {
	N f1 = f(0);
	N f0 = f(1);
	printf("%ld %ld\n", f0, f1);
}