#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::{
	convert::AsRef,
	ffi::CStr,
	fmt::{Debug, Display},
	fs, io,
	os::unix::prelude::AsRawFd,
	path::Path,
};

use anyhow::Result;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use smallstr::SmallString;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

static C_PRINTING_LOCK: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

#[derive(PartialEq, Eq)]
pub struct AST(Exp);

impl AST {
	pub fn new(p: impl AsRef<Path>) -> Result<Self> {
		let file = fs::File::open(p)?;
		let fd = file.as_raw_fd();
		// Safety: file descriptor is held until the end of the scope,
		// resulting AST does not rely on it remaining alive.
		// Fd <-> Raw pointer is just a C typing difference
		let c_ast = unsafe { pExp(fd as *mut _) };
		Ok(Self(c_ast))
	}

	pub fn from_stdin() -> Self {
		let rs_stdin = io::stdin();
		let lock = rs_stdin.lock();
		let fd = lock.as_raw_fd();
		// Safety: file descriptor is held until the end of the scope,
		// resulting AST does not rely on it remaining alive.
		// Fd <-> Raw pointer is just a C typing difference
		let c_ast = unsafe { pExp(fd as *mut _) };
		Self(c_ast)
	}
}

impl Drop for AST {
	fn drop(&mut self) {
		// Safety: Just a wrapper around the C destructor.
		// No raw pointers are ever handed out in Rust-land
		unsafe {
			free_Exp(self.0);
		}
	}
}

impl Display for AST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let _lock = C_PRINTING_LOCK.lock();
		// Safety: C_PRINTING_LOCK accquired, pointer is RAII guarded
		unsafe {
			let cstr = CStr::from_ptr(showExp(self.0));
			let string = cstr.to_string_lossy();
			write!(f, "{string}")
		}
	}
}

impl Debug for AST {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let _lock = C_PRINTING_LOCK.lock();
		// Safety: C_PRINTING_LOCK accquired, pointer is RAII guarded
		unsafe {
			let cstr = CStr::from_ptr(printExp(self.0));
			let string = cstr.to_string_lossy();
			write!(f, "{string}")
		}
	}
}
