#pragma once

#include "llvm_types.h"
#include <iostream>
#include <unordered_map>

extern "C" void collect_garbage();
extern "C" void *allocate(usize size);
