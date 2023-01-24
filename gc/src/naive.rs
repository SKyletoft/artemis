use std::{
	alloc::{self, Layout},
	collections::{HashMap, HashSet},
	mem, slice,
};

use once_cell::sync::Lazy;
use parking_lot::Mutex;

static ALLOCATIONS: Lazy<Mutex<HashMap<usize, Layout>>> = Lazy::new(|| Mutex::new(HashMap::new()));
static mut STACK_START: usize = 0;

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

	eprintln!("  allocating: {}", ptr as usize);
	ptr
}

/// A depth-first search algorithm going through everything on the stack and everything it points at to create a
/// collection of all living objects and then just remove everything that isn't on there
#[no_mangle]
pub extern "C" fn collect_garbage() {
	let allocations = &mut *ALLOCATIONS.lock();
	let stack = get_stack();

	let mut objects_to_check = stack
		.iter()
		.copied()
		.filter(|word| allocations.contains_key(word))
		.collect::<Vec<_>>();
	// eprintln!("----------------------------------------");
	// eprintln!("objects_to_check: {objects_to_check:#?}");
	// eprintln!("stack: {stack:#?}");
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

		for &word in object.iter().filter(|&word| allocations.contains_key(word)) {
			objects_to_check.push(word);
		}
	}

	// Revert to drain when `HashMap::drain_filter` gets stabilised
	#[allow(clippy::needless_collect)] // Incorrect lint, suggestion does not compile
	let to_remove = allocations
		.keys()
		.copied()
		.filter(|adr| !found_objects.contains(adr))
		.collect::<Vec<_>>();

	for ptr in to_remove.into_iter() {
		eprintln!("deallocating: {ptr}");
		let layout = allocations.remove(&ptr).unwrap();
		unsafe {
			alloc::dealloc(ptr as _, layout);
		}
	}
}

// This is a macro to force inlining. Without inlining the stack pointer value will be for yet another inner scope
macro_rules! get_stack_pointer {
	() => {{
		#[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
		compile_error!("GC only supports x64 and aarch64 right now");

		if cfg!(target_arch = "x86_64") {
			x86::bits64::registers::rsp() as _
		} else if cfg!(target_arch = "aarch64") {
			0 //aarch64::regs::SP.read() as _
		} else {
			unreachable!()
		}
	}};
}

/// Sets the start of the stack for the Garbage Collector and returns the old value
/// # Safety
/// Needs to be run in a scope where the gc will never be run after it returns (unless
/// this is set again)
#[no_mangle]
#[inline(always)]
pub unsafe extern "C" fn reset_stack_start() -> usize {
	let old = STACK_START;
	STACK_START = get_stack_pointer!();
	old
}

fn get_stack() -> &'static [usize] {
	let start = unsafe { STACK_START };
	let end = get_stack_pointer!();
	let len = (start - end + mem::size_of::<usize>()) / mem::size_of::<usize>();
	assert_eq!((start - end) % 8, 0);

	// Safety: Yeah, I don't trust this yet. It assumes the stack is contiguous and that STACK_START was initialised properly
	// Also requires that the program is single threaded
	// Note that the stack is backwards as it grows downwards
	unsafe { slice::from_raw_parts(end as *const usize, len) }
}

#[cfg(test)]
#[allow(dead_code)]
mod test {

	use crate::naive::*;

	#[inline(never)]
	fn allocate_in_other_scope() -> *mut u8 {
		allocate(1)
	}

	#[inline(never)]
	fn actual_test() {
		let mut pointers = [8888888888888usize, 0, 0, 0, 9999999999999];
		let pointers = &mut pointers as *mut _ as *mut usize;

		unsafe {
			pointers.offset(1)
				.write_volatile(allocate_in_other_scope() as usize);
			dbg!(&ALLOCATIONS);
			assert_eq!(ALLOCATIONS.lock().len(), 1);

			pointers.offset(2)
				.write_volatile(allocate_in_other_scope() as usize);
			dbg!(&ALLOCATIONS);
			assert_eq!(ALLOCATIONS.lock().len(), 2);

			pointers.offset(3)
				.write_volatile(allocate_in_other_scope() as usize);
			dbg!(&ALLOCATIONS);
			assert_eq!(ALLOCATIONS.lock().len(), 3);

			pointers.offset(1).write_volatile(0);
			pointers.offset(2).write_volatile(0);
			pointers.offset(3).write_volatile(0);

			let stack = get_stack();
			dbg!(stack);

			pointers.write_volatile(allocate_in_other_scope() as usize);
			assert_eq!(ALLOCATIONS.lock().len(), 1);
		}
	}

	/*
	#[test]
	fn allocate_a_bit() {
		unsafe {
			reset_stack_start();
		}
		actual_test();
	}
	*/
}
