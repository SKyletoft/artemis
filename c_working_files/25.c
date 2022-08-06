#include <stdint.h>

int64_t f(uint64_t a) {
	const int64_t z = 5 + 2;
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
