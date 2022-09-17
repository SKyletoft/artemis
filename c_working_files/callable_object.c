#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef int64_t (*FunctionPointer)(); // Variadic, not (void)

typedef struct {
	FunctionPointer function;
	int64_t count;
	// Must always contain enough space for all the arguments of
	// the underlying function, not how many are actually applied
	int64_t *args;
} Callable;

typedef struct {
	int64_t count;
	Callable **functions;
	int64_t *first;
} ComposedFunction;

int64_t call_x64_callable_with_stack(
	FunctionPointer function,
	int64_t count,
	int64_t *args
);

int64_t call_x64_callable_no_stack(
	FunctionPointer function,
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

int64_t call_x64_callable_by_pointer(Callable *c) {
	return call_x64_callable(*c);
}

int64_t call_composed_function(ComposedFunction f) {
	int64_t last_ret = 0;
	if (f.first) {
		last_ret = *f.first;
	}
	for (ssize_t i = 0; i < f.count; i++) {
		Callable c = *f.functions[i];

		c.args[c.count] = last_ret;
		c.count++;

		last_ret = call_x64_callable(c);
	}
	return last_ret;
}
