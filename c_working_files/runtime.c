#include <stdio.h>
#include <stdint.h>

int64_t f(uint64_t b);
uint8_t *allocate(uint64_t size);
void collect_garbage();

int64_t c_f(uint64_t a) {
	const int64_t z = 5 + 2; // + 1 + (1 ? 10 : 20);
	// const int64_t w = 2 + z;
	int64_t x = 3 + 3;
	int64_t y;
	if (a) {
		x = 2 + 1 * x - 43;
		y = 1 + 2;
	} else {
		x = 3 + 1;
		y = 2 + 3;
	}
	return x + y + z;
}

int main() {
	int64_t a = f(0);
	int64_t b = f(1);

	// uint8_t* ptr = allocate(1024);
	uint8_t* ptr = 0;

	printf("artemis: %ld, %ld %p\n", a, b, ptr);

	a = g(0);
	b = g(1);

	printf("c:       %ld, %ld %p\n", a, b, ptr);

	return 0;
}
