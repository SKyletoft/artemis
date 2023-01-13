use std::{error, fmt};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Error {
	WrongRegisterType(u32),
	UnconvertedBlock(u32),
	BlockWithoutReturn(u32),
	PathsDoNotMerge(u32),
	ReturnedNonExistantBlock(u32),
	MissingRegister(u32),
	MismatchedRegisterTypes(u32),
	InvalidIR(u32),
	Unsupported(u32),
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			WrongRegisterType(line) => write!(f, "[{line}] Wrong Register Type (ex. GP vs FP)"),
			UnconvertedBlock(line) => write!(f, "[{line}] Unconverted block, some block in the generated code is unreachable"),
			BlockWithoutReturn(line) => write!(f, "[{line}] Block doesn't contain a return statement"),
			PathsDoNotMerge(line) => write!(f, "[{line}] Paths don't merge"),
			ReturnedNonExistantBlock(line) => write!(f, "[{line}] Returned an end index to a block that doesn't exist (out of bounds or None)"),
			MissingRegister(line) => write!(f, "[{line}] Value of return value from block is not in the active register bank"),
			MismatchedRegisterTypes(line) => write!(f, "[{line}] Registers weren't both GP or both FP"),
			InvalidIR(line) => write!(f, "[{line}] Invalid IR (GP instruction with FP register?)"),
			Unsupported(line) => write!(f, "[{line}] Action is unsupported on this platform"),
		}
	}
}

impl error::Error for Error {}
