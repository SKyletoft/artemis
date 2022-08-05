#include "gc.h"

using Layout = usize;

static std::unordered_map<usize, Layout> ALLOCATIONS{};

extern "C" void *allocate(usize size) {
	collect_garbage();

	void *const ptr = malloc(size);
	if (ptr == nullptr || ((usize) ptr % 8 != 0)) {
		std::cerr << "Allocation failure\n";
		exit(-1);
	}

	const usize int_ptr = (usize) ptr;
	if (ALLOCATIONS.contains(int_ptr)) {
		std::cerr << "malloc returned already allocated pointer\n";
		exit(-1);
	}

	ALLOCATIONS.emplace(std::make_pair(int_ptr, size));

	return ptr;
}

extern "C" void collect_garbage() {
}
