#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint64_t N;
typedef double R;

typedef struct {
	N tag;
	union {
		N n;
		R r;
	};
} T;

N __art_main(N x);
N __art_f(T t);
int main(int argc, char **argv);

N __art_main(N x) {
	return x * __art_f((T) {
		.tag = 1,
		.r = 2.0,
	});
}

N __art_f(T t) {
	if (t.tag == 0) {
		return t.n;
	} else if (t.tag == 1) {
		N n = (N) t.r;
		return n + 1ULL;
	}
	exit(-1);
}

int main(int argc, char **argv) {
	if (argc != 2) {
		puts("Input error");
		exit(-1);
	}
	N n = atoi(argv[1]);
	N ret = __art_main(n);
	printf("%lu\n", ret);
}
