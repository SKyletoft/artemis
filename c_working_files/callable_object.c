#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
	int64_t (*function)(); // Variadic, not (void)
	int64_t count;
	int64_t *args;
} Callable;

typedef struct {
	int64_t  count;
	Callable **functions;
	int64_t  *first;
} ComposedFunction;

int64_t call_x64_callable_with_stack(
	int64_t (*function)(),
	int64_t count,
	int64_t *args
);

int64_t call_x64_callable_no_stack(
	int64_t (*function)(),
	int64_t count,
	int64_t *args
);

int64_t call_x64_callable(Callable c) {
	if (c.count > 6) {
		return call_x64_callable_with_stack(c.function, c.count, c.args);
	} else {
		return call_x64_callable_no_stack(c.function, c.count, c.args);
	}
}

const char* long_str =
	"%ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld\n";
const char* short_str =
	"%ld %ld %ld %ld\n";

int main() {
	int64_t xs[15] = {
		(int64_t) long_str,
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

	int64_t ys[5] = {
		(int64_t) short_str,
		1,
		2,
		3,
		4,
	};

	Callable long_printf_callable = (Callable){
		.function = &printf,
		.count    = 15,
		.args     = xs,
	};

	Callable short_printf_callable = (Callable){
		.function = &printf,
		.count    = 5,
		.args     = ys,
	};

	int64_t correct1 = printf(short_str, 1, 2, 3, 4);
	int64_t correct2 = printf(long_str, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14);

	int64_t ret1 = call_x64_callable(short_printf_callable);
	int64_t ret2 = call_x64_callable(long_printf_callable);

	printf("%ld (%ld) %ld (%ld)\n", ret1, correct1, ret2, correct2);
}
