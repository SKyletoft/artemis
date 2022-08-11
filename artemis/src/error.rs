use std::{error, fmt};

#[derive(Debug, Copy, Clone)]
pub enum Error {
	ParseError(u32),
	Internal(u32),
	TypeError(u32),
	InvalidTarget(u32),
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self:?}")
	}
}

impl error::Error for Error {}
