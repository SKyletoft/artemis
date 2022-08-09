use std::fmt;

type SmallString = smallstr::SmallString<[u8; 16]>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GeneralPurposeRegister {
	RAX,
	RBX,
	RCX,
	RDX,
	RSI,
	RDI,
	RSP,
	RBP,
	R8,
	R9,
	R10,
	R11,
	R12,
	R13,
	R14,
	R15,
	RIP,
}

impl fmt::Display for GeneralPurposeRegister {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let reg = match self {
			GeneralPurposeRegister::RAX => "rax",
			GeneralPurposeRegister::RBX => "rbx",
			GeneralPurposeRegister::RCX => "rcx",
			GeneralPurposeRegister::RDX => "rdx",
			GeneralPurposeRegister::RSI => "rsi",
			GeneralPurposeRegister::RDI => "rdi",
			GeneralPurposeRegister::RSP => "rsp",
			GeneralPurposeRegister::RBP => "rbp",
			GeneralPurposeRegister::R8 => " r8",
			GeneralPurposeRegister::R9 => " r9",
			GeneralPurposeRegister::R10 => "r10",
			GeneralPurposeRegister::R11 => "r11",
			GeneralPurposeRegister::R12 => "r12",
			GeneralPurposeRegister::R13 => "r13",
			GeneralPurposeRegister::R14 => "r14",
			GeneralPurposeRegister::R15 => "r15",
			GeneralPurposeRegister::RIP => "rip",
		};
		write!(f, "{reg}")
	}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatingPointRegister {
	XMM0,
	XMM1,
	XMM2,
	XMM3,
	XMM4,
	XMM5,
	XMM6,
	XMM7,
	XMM8,
	XMM9,
	XMM10,
	XMM11,
	XMM12,
	XMM13,
	XMM14,
	XMM15,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
	Global(SmallString),
	Add(GeneralPurposeRegister, GeneralPurposeRegister),
	Sub(GeneralPurposeRegister, GeneralPurposeRegister),
	Imul(GeneralPurposeRegister, GeneralPurposeRegister),
	Push(GeneralPurposeRegister),
	Pop(GeneralPurposeRegister),
	Mov(GeneralPurposeRegister, GeneralPurposeRegister),
	MovLit(GeneralPurposeRegister, u64),
	Div(GeneralPurposeRegister),
	Idiv(GeneralPurposeRegister),
	And(GeneralPurposeRegister, GeneralPurposeRegister),
	Or(GeneralPurposeRegister, GeneralPurposeRegister),
	Xor(GeneralPurposeRegister, GeneralPurposeRegister),
	Test(GeneralPurposeRegister, GeneralPurposeRegister),
	Xchg(GeneralPurposeRegister, GeneralPurposeRegister),
	Label(SmallString),
	Je(SmallString),
	Jne(SmallString),
	Jmp(SmallString),
	Lea(
		GeneralPurposeRegister,
		GeneralPurposeRegister,
		GeneralPurposeRegister,
	),
	Ret,
}
use Instruction::*;

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct AssemblyBuilder(Vec<Instruction>);

impl AssemblyBuilder {
	pub fn new() -> Self {
		Self(Vec::new())
	}

	pub fn global(&mut self, name: &str) {
		self.0.push(Instruction::Global(SmallString::from(name)))
	}

	pub fn add(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Add(l, r))
	}

	pub fn sub(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Sub(l, r))
	}

	pub fn imul(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Imul(l, r))
	}

	pub fn push(&mut self, l: GeneralPurposeRegister) {
		self.0.push(Push(l))
	}

	pub fn pop(&mut self, l: GeneralPurposeRegister) {
		self.0.push(Pop(l))
	}

	pub fn mov(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Mov(l, r))
	}

	pub fn mov_lit(&mut self, l: GeneralPurposeRegister, r: u64) {
		self.0.push(MovLit(l, r))
	}

	pub fn div(&mut self, l: GeneralPurposeRegister) {
		self.0.push(Div(l))
	}

	pub fn idiv(&mut self, l: GeneralPurposeRegister) {
		self.0.push(Idiv(l))
	}

	pub fn and(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(And(l, r))
	}

	pub fn or(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Or(l, r))
	}

	pub fn xor(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Xor(l, r))
	}

	pub fn test(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Test(l, r))
	}

	pub fn label(&mut self, l: SmallString) {
		self.0.push(Label(l))
	}

	pub fn jne(&mut self, l: SmallString) {
		self.0.push(Jne(l))
	}

	pub fn je(&mut self, l: SmallString) {
		self.0.push(Je(l))
	}

	pub fn jmp(&mut self, l: SmallString) {
		self.0.push(Jmp(l))
	}

	pub fn lea(
		&mut self,
		a: GeneralPurposeRegister,
		b: GeneralPurposeRegister,
		c: GeneralPurposeRegister,
	) {
		self.0.push(Lea(a, b, c))
	}

	pub fn xchg(&mut self, l: GeneralPurposeRegister, r: GeneralPurposeRegister) {
		self.0.push(Xchg(l, r))
	}

	pub fn ret(&mut self) {
		self.0.push(Ret)
	}
}

impl Default for AssemblyBuilder {
	fn default() -> Self {
		Self::new()
	}
}

impl fmt::Display for AssemblyBuilder {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for function_name in self.0.iter().filter_map(|x| {
			if let Instruction::Global(y) = x {
				Some(y)
			} else {
				None
			}
		}) {
			writeln!(f, "global\t{function_name}")?;
		}
		writeln!(f, "section .text")?;
		for line in self.0.iter() {
			match line {
				Global(_) => {}
				Label(l) => writeln!(f, "{l}:")?,
				Add(l, r) => writeln!(f, "\tadd\t{l}, {r}")?,
				Sub(l, r) => writeln!(f, "\tsub\t{l}, {r}")?,
				Imul(l, r) => writeln!(f, "\timul\t{l}, {r}")?,
				Push(l) => writeln!(f, "\tpush\t{l}")?,
				Pop(l) => writeln!(f, "\tpop\t{l}")?,
				Mov(l, r) => writeln!(f, "\tmov\t{l}, {r}")?,
				MovLit(l, r) => writeln!(f, "\tmov\t{l}, {r:3}")?,
				Div(l) => writeln!(f, "\tdiv\t{l}")?,
				Idiv(l) => writeln!(f, "\tidiv\t{l}")?,
				And(l, r) => writeln!(f, "\tand\t{l}, {r}")?,
				Or(l, r) => writeln!(f, "\tor\t{l}, {r}")?,
				Xor(l, r) => writeln!(f, "\txor\t{l}, {r}")?,
				Test(l, r) => writeln!(f, "\ttest\t{l}, {r}")?,
				Je(l) => writeln!(f, "\tje\t{l}")?,
				Jne(l) => writeln!(f, "\tjne\t{l}")?,
				Jmp(l) => writeln!(f, "\tjmp\t{l}")?,
				Xchg(l, r) => writeln!(f, "\txchg\t{l}, {r}")?,
				Lea(a, b, c) => writeln!(f, "\tlea\t{a}, {b}[{c}]")?,
				Ret => writeln!(f, "\tret")?,
			}
		}
		Ok(())
	}
}
