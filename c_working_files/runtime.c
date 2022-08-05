#include <stdio.h>
#include <stdint.h>

int64_t f(uint64_t b);

int main() {
	int64_t a = f(0);
	int64_t b = f(1);

	printf("%ld, %ld\n", a, b);

	return 0;
}
