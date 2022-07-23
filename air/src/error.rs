use std::{error, fmt};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Error {
	WrongRegisterType,
	UnconvertedBlock,
	BlockWithoutReturn,
	PathsDoNotMerge,
	ReturnedNonExistantBlock,
	MissingRegister,
	MismatchedRegisterTypes,
	InvalidIR,
	Unsupported,
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			WrongRegisterType => write!(f, "Wrong Register Type (ex. GP vs FP)"),
			UnconvertedBlock => write!(f, "Unconverted block, some block in the generated code is unreachable"),
			BlockWithoutReturn => write!(f, "Block doesn't contain a return statement"),
			PathsDoNotMerge => write!(f, "Paths don't merge"),
			ReturnedNonExistantBlock => write!(f, "Returned an end index to a block that doesn't exist (out of bounds or None)"),
			MissingRegister => write!(f, "Value of return value from block is not in the active register bank"),
			MismatchedRegisterTypes => write!(f, "Registers weren't both GP or both FP"),
			InvalidIR => write!(f, "Invalid IR (GP instruction with FP register?)"),
			Unsupported => write!(f, "Action is unsupported on this platform"),
		}
	}
}

impl error::Error for Error {}
