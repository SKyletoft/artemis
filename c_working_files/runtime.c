#include <stdio.h>
#include <stdint.h>

int64_t f(uint64_t b);
uint8_t *allocate(uint64_t size);
void collect_garbage();

int main() {
	int64_t a = f(0);
	int64_t b = f(1);

	// uint8_t* ptr = allocate(1024);
	uint8_t* ptr = 0;

	printf("%ld, %ld %p\n", a, b, ptr);

	return 0;
}
