#include <stdio.h>
#include <stdint.h>

uint64_t f(uint64_t b);
uint8_t *allocate(uint64_t size);
void collect_garbage();

uint64_t g_c(uint64_t a) {
	return a * 4;
}

uint64_t h_c(uint64_t a) {
	return a * 5;
}

uint64_t f_c(uint64_t a) {
	return g_c(a + 3) + h_c(a + 2);
}

int64_t print_n(uint64_t a) {
	return printf("%ld\n", a);
}

int64_t print_z(int64_t a) {
	return printf("%ld\n", a);
}

int64_t print_r(double a) {
	return printf("%lf\n", a);
}

int64_t print_b(int64_t a) {
	char const * const str = a ? "true" : "false";
	puts(str);
	return a;
}

int main() {
	int64_t a = f(0);
	int64_t b = f(1);

	printf("%ld, %ld\n", a, b);

	return 0;
}
