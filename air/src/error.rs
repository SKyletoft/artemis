use std::{error, fmt};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Error {
	WrongRegisterType,
	UnconvertedBlock,
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			WrongRegisterType => write!(f, "Wrong Register Type (ex. GP vs FP)"),
			UnconvertedBlock => write!(f, "Unconverted block, some block in the generated code is unreachable"),
		}
	}
}

impl error::Error for Error {}
