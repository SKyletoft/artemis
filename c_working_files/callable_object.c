#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
	int64_t (*function)(); // Variadic, not (void)
	int64_t count;
	int64_t *args;
} Callable;

typedef struct {
	int64_t count;
	Callable *functions;
	int64_t *first;
} ComposedFunction;

int64_t call_x64_callable_with_stack(Callable c);
int64_t call_x64_callable_no_stack(
	int64_t (*function)(),
	int64_t count,
	int64_t *args
);

int64_t call_x64_callable(Callable c) {
	if (c.count > 6) {
		return  call_x64_callable_with_stack(c);
	} else {
		return call_x64_callable_no_stack(c.function, c.count, c.args);
	}
}

const char* str = "%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld\n";

int main() {
	int64_t xs[15] = {
		(int64_t) str,
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		10,
		11,
		12,
		13,
		14
	};

	Callable printf_callable = (Callable){
		.function = &printf,
		.count    = 15,
		.args     = xs,
	};

	printf(str, 1, 2, 3,
		4,
		5,
		6,
		7,
		8,
		9,
		10,
		11,
		12,
		13,
		14
	);

	int64_t ret = call_x64_callable(printf_callable);

	printf("%ld\n", ret);
}
