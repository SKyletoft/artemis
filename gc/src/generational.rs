use std::{
	alloc::{self, Layout},
	cell::UnsafeCell,
	collections::{HashMap, HashSet},
	mem, slice,
};

use once_cell::sync::Lazy;
use parking_lot::Mutex;

const SHORT_HEAP_SIZE: usize = 1024;

static mut SHORT_INDEX: usize = 0;
static mut SHORT_HEAP: [UnsafeCell<usize>; SHORT_HEAP_SIZE] =
	unsafe { mem::transmute([0usize; SHORT_HEAP_SIZE]) };

static HEAP_INDEX: Lazy<Mutex<HashMap<usize, usize>>> = Lazy::new(|| Mutex::new(HashMap::new()));

static mut STACK_START: usize = 0;

#[inline(always)]
fn get_stack_pointer() -> usize {
	x86::bits64::registers::rsp() as usize
}

#[inline(never)]
#[no_mangle]
extern "C" fn alloc(size: usize) -> *mut usize {
	let stack_end = get_stack_pointer();
	allocate(size, stack_end)
}

#[inline(never)]
fn allocate(size: usize, stack_end: usize) -> *mut usize {
	let (bump, idx) = unsafe { (&SHORT_HEAP, SHORT_INDEX) };
	if idx == SHORT_HEAP_SIZE {
		let stack =
			unsafe { slice::from_raw_parts(STACK_START as _, stack_end - STACK_START) };
		clear_bump_heap(stack);
	}

	unsafe {
		let next = &bump[SHORT_INDEX];
		SHORT_INDEX += 1;
		next.get()
	}
}

fn clear_bump_heap(stack: &[usize]) {
	let bump: HashSet<usize> =
		unsafe { SHORT_HEAP.as_slice().iter().map(|x| *x.get()).collect() };

	let survivors: Vec<_> = stack
		.iter()
		.map(|x| (x as *const _, x))
		.filter(|(_, x)| bump.contains(x))
		.collect();

	for survivor in survivors.iter() {
		allocate_heap(&[]);
	}
	dbg!(&survivors);
}

fn allocate_heap(data: &[usize]) -> *mut usize {
	let layout = Layout::array::<usize>(data.len())
		.expect("Allocation failure: Allocation too large");

	let ptr = unsafe { alloc::alloc(layout) } as *mut usize;
	if ptr.is_null() {
		panic!("Allocation failure: Out of memory");
	}

	for (idx, &word) in data.iter().enumerate() {
		unsafe {
			ptr.add(idx).write(word);
		}
	}

	let mut heap_index = HEAP_INDEX.lock();
	heap_index.insert(ptr as _, data.len());

	ptr
}
