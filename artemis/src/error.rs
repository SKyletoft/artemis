use std::{error, fmt};

#[derive(Debug, Clone)]
pub enum Error {
	OpAssignOnPattern(u32),
	ParseError(u32),
	Internal(u32),
	TypeError(u32),
	InvalidTarget(u32),
	NoInputFiles(u32),
	External(String),

	TypeNonFunctionAsFunction(u32),
	ForbiddenExprAtTopLevel(u32),
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			OpAssignOnPattern(l) => write!(f, "[{l}] Cannot assign to a pattern"),
			NoInputFiles(l) => write!(f, "[{l}] No input files provided"),
			External(s) => write!(f, "{s}"),
			_ => write!(f, "{self:?}"),
		}
	}
}

impl error::Error for Error {}
