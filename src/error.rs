use core::fmt;
use std::error;

#[derive(Debug, Copy, Clone)]
pub enum Error {
	ParseError,
	Internal,
	TypeError,
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self:?}")
	}
}

impl error::Error for Error {}
