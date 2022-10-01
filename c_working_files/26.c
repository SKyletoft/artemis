#include <stdio.h>

typedef unsigned long N;
typedef signed long Z;

Z print_n(N a) {
	return printf("%d\n", a);
}

N g (N b) { return b * 4; }

N h (N a, N b, N c, N d) {return a + b * c - d;}

N i (N a, N b, N c, N d, N e, N f, N g, N h) {return a + b * c - d;}

N l (N a, N b, N c, N d, N e, N f, N g, N h, N i) {return a + b * c - d;}

N j (N x) {return x;}

N k (N x) {
	return x + 5;
}

N f(N a) {
	N a2 = g(a);
	N a3 = h(a+ 2, 1, 2, 3);
	N a4 = i(1, 2, 3, 4, 5, 6, 7, 8);
	N a5 = j(8);
	N a6 = k(2);
	N a7 = l(1, 2, 3, 4, 5, 6, 7, 8, 9);

	print_n(a2);
	print_n(a3);
	print_n(a4);
	print_n(a5);
	print_n(a6);
	print_n(a7);

	return a2 + a3 + a4 + a5 + a6 + a7;
}

int main() {
	N f0 = f(0);
	N f1 = f(1);
	printf("%ld, %ld\n", f0, f1);
}
