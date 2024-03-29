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
	RequireTypeAtTopLevel(u32),
	MismatchedTypes(u32),
	InternalUnhandledTypeAliasConversion(u32),
	UndefinedTypeAlias(u32),
	TopLevelTypeAscription(u32),
	DuplicateFunctionDefinition(u32),
	IncorrectTypeAscription(u32),
	MutableTypeAlias(u32),
	ConditionIsntBoolean(u32),
	UndefinedVariable(u32),
	UndefinedFunction(u32),
	PatternDoesntMatch(u32),
	UnprovedIrrefutablePattern(u32),
	AssignmentToUndeclaredVariable(u32),
	AssignmentToConst(u32),
	TODOUnsupportedAssignmentPattern(u32),
	InternalMismatchedTypes(u32),
	InternalIllegalConstructAtTopLevel(u32),
	EmptyAssignment(u32),
	InternalCheckedUndefinedVariable(u32),
	InternalCheckedMismatchedTypes(u32),
	InternalCallOfUnknownFunction(u32),
	NoCasesInMatch(u32),
	UnknownStructField(u32),
	NotAStructType(u32),
	TODONotYetSupportedPatternMatch(u32),
	InternalNonNameDot(u32),
	TODOAccessFieldOnEnum(u32),
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
