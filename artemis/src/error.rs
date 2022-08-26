use std::{error, fmt};

#[derive(Debug, Clone)]
pub enum Error {
	ParseError(u32),
	Internal(u32),
	TypeError(u32),
	InvalidTarget(u32),
	NoInputFiles(u32),
	External(String),
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			NoInputFiles(l) => write!(f, "[{l}] No input files provided"),
			External(s) => write!(f, "{s}"),
			_ => write!(f, "{self:?}"),
		}
	}
}

impl error::Error for Error {}
