use std::{
	alloc::{self, Layout},
	collections::{HashMap, HashSet},
	mem, slice,
};

use once_cell::sync::Lazy;
use parking_lot::Mutex;

static ALLOCATIONS: Lazy<Mutex<HashMap<usize, Layout>>> = Lazy::new(|| Mutex::new(HashMap::new()));
static STACK_START: Lazy<usize> = Lazy::new(get_stack_pointer);

#[no_mangle]
pub extern "C" fn allocate(size: usize) -> *mut u8 {
	//TODO: Should GC run on *every* allocation?
	collect_garbage();

	let layout = Layout::from_size_align(size, mem::align_of::<usize>())
		.expect("Tried to allocate too much");
	let ptr = unsafe { alloc::alloc(layout) };
	if ptr.is_null() {
		panic!("Allocation failure!");
	}

	let int_ptr = ptr as usize;
	let allocations = &mut *ALLOCATIONS.lock();
	assert!(!allocations.contains_key(&int_ptr));
	allocations.insert(int_ptr, layout);

	ptr
}

/// A depth-first search algorithm going through everything on the stack and everything it points at to create a
/// collection of all living objects and then just remove everything that isn't on there
#[no_mangle]
pub extern "C" fn collect_garbage() {
	let start = *STACK_START;
	let end = get_stack_pointer();
	let allocations = &mut *ALLOCATIONS.lock();

	// Safety: Yeah, I don't trust this yet. It assumes the stack is contiguous and that STACK_START was initialised properly
	// Also requires that the program is single threaded
	let stack = unsafe { slice::from_raw_parts(start as *const usize, (end - start) as usize) };

	let mut objects_to_check = stack
		.iter()
		.copied()
		.filter(|word| allocations.contains_key(word))
		.collect::<Vec<_>>();
	let mut found_objects = HashSet::new();

	while let Some(adr) = objects_to_check.pop() {
		let is_new = found_objects.insert(adr);
		if !is_new {
			continue;
		}

		let size = allocations[&adr];
		// Safety: Layout isn't rounded up because we're always working in words
		let object = unsafe {
			slice::from_raw_parts(
				adr as *const usize,
				size.size() / mem::size_of::<usize>(),
			)
		};

		for &word in object
			.iter()
			.filter(|&word| allocations.contains_key(word))
		{
			objects_to_check.push(word as usize);
		}
	}

	// Revert to drain when `HashMap::drain_filter` gets stabilised
	let to_remove = allocations
		.keys()
		.copied()
		.filter(|adr| found_objects.contains(adr))
		.collect::<Vec<_>>();

	for ptr in to_remove.into_iter() {
		let layout = allocations.remove(&ptr).unwrap();
		unsafe {
			alloc::dealloc(ptr as _, layout);
		}
	}
}

#[inline(always)]
fn get_stack_pointer() -> usize {
	#[cfg(target_arch = "x86_64")]
	return x86::bits64::registers::rsp() as _;

	#[cfg(target_arch = "aarch64")]
	return aarch64::regs::SP.read();

	#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
	compile_error!("GC only supports x64 and aarch64 right now");
}
