use std::{io, os::unix::io::AsRawFd, ffi::CStr};


use smallstr::SmallString;
use smallvec::{smallvec, SmallVec};

pub mod bindings;

fn main() {
	unsafe {
		let stdin = io::stdin();
		let lock = stdin.lock();
		let c_compatible_stdin = lock.as_raw_fd();

		let parse_tree = bindings::pExp(c_compatible_stdin as *mut _);

		let s = CStr::from_ptr(bindings::showExp(parse_tree)).to_string_lossy();
		println!("{s}");

		let s = CStr::from_ptr(bindings::printExp(parse_tree)).to_string_lossy();
		println!("{s}");

		bindings::free_Exp(parse_tree);
	}
}
