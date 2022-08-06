use anyhow::{bail, Result};
use FloatingPointRegister::*;
use GeneralPurposeRegister::*;

type SmallString = smallstr::SmallString<[u8; 16]>;

use crate::{
	error::Error,
	register_allocation::{
		BinOp, Block, BlockEnd, CodeConstruct, Expression, FunctionCall, Op, Register, UnOp,
	},
	simplify::BlockId,
	x86_64_definition::{
		AssemblyBuilder, FloatingPointRegister, GeneralPurposeRegister, Instruction,
	},
};

// SYSTEMV ABI for x64
// Stack must be 16-byte aligned and grows downwards
// Arguments go in rdi, rsi, rdx, rcx, r8, r9
// Preserve rbx, rsp, rbp, r12, r13, r14, r15
// Don't care about rax, rdi, rsi, rdz, rcx, r8, r9, r10, r11
// Return value in rax (+ higher bits in rdx for 128 bit values)

/// General purpose registers, in order of priority
const GP: [GeneralPurposeRegister; 14] = {
	[
		RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11, RBX, R12, R13, R14, R15,
	]
};

/// General purpose registers, in order of priority
const FP: [FloatingPointRegister; 16] = {
	[
		XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12,
		XMM13, XMM14, XMM15,
	]
};

pub fn assemble(construct: &CodeConstruct) -> Result<String> {
	match construct {
		CodeConstruct::Function { name, blocks } => assemble_function(name, blocks),
		CodeConstruct::Variable { .. } => todo!(),
		CodeConstruct::ImmediateExpression { .. } => todo!(),
	}
}

fn assemble_function(name: &str, blocks: &[Block]) -> Result<String> {
	let mut assembler = AssemblyBuilder::new();
	assembler.global(name);
	assembler.label(SmallString::from(name));

	for (idx, block) in blocks.iter().enumerate() {
		assemble_block(block, idx.into(), &mut assembler, name)?;
	}

	let res = format!("{assembler}");
	Ok(res)
}

fn assemble_block(
	Block { block, out }: &Block,
	id: BlockId,
	assembler: &mut AssemblyBuilder,
	name: &str,
) -> Result<()> {
	assembler.label(id.label(name)?);
	for line in block {
		match line {
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op: Op::Div,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) =>
			{
			todo!()
			}
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op: Op::UDiv,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) => 
			{
				todo!("I hate x86 division so much");
			}
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) if t == l => match op {
				Op::Add => assembler.add(GP[l], GP[r]),
				Op::Sub => assembler.sub(GP[l], GP[r]),
				Op::Abs => todo!(),
				Op::Mul => assembler.imul(GP[l], GP[r]),
				Op::And => assembler.and(GP[t], GP[r]),
				Op::Or => assembler.or(GP[l], GP[r]),
				Op::Xor => assembler.xor(GP[l], GP[r]),
				Op::Not => todo!(),
				Op::StoreMem => todo!(),
				Op::LoadMem => todo!(),
				Op::Move => todo!(),
				Op::Div | Op::UDiv => unreachable!("Covered by outer pattern"),
				Op::Swap | Op::FAdd | Op::FSub | Op::FAbs | Op::FMul | Op::FDiv => {
					bail!(Error::InvalidIR)
				}
			},
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) => match op {
				Op::Add => assembler.lea(GP[t], GP[l], GP[r]),
				Op::Sub => {
					assembler.mov(GP[t], GP[l]);
					assembler.sub(GP[t], GP[r])
				}
				Op::Mul => {
					assembler.mov(GP[t], GP[l]);
					assembler.imul(GP[t], GP[r])
				}
				Op::Div => {
					assembler.mov(GP[t], GP[l]);
					// assembler.idiv(GP[t], GP[r])?
					todo!()
				}
				Op::UDiv => {
					assembler.mov(GP[t], GP[l]);
					// assembler.div(GP[t], GP[r])?
					todo!()
				}
				Op::And => {
					assembler.mov(GP[t], GP[l]);
					assembler.and(GP[t], GP[r]);
				}
				Op::Or => {
					assembler.mov(GP[t], GP[l]);
					assembler.or(GP[t], GP[r]);
				}
				Op::Xor => {
					assembler.mov(GP[t], GP[l]);
					assembler.xor(GP[t], GP[r])
				}
				Op::StoreMem => {
					assembler.mov(GP[t], GP[l]);
					// assembler.imul(GP[t], GP[r])?
					todo!()
				}
				Op::LoadMem => {
					assembler.mov(GP[t], GP[l]);
					// assembler.imul(GP[t], GP[r])?
					todo!()
				}
				Op::Move => {
					// Is this really ok?
					// assembler.mov(GP[t], GP[l] , GP[r])
					todo!()
				}

				Op::Swap
				| Op::Abs
				| Op::Not
				| Op::FAdd
				| Op::FSub
				| Op::FAbs
				| Op::FMul
				| Op::FDiv => {
					bail!(Error::InvalidIR)
				}
			},
			&Expression::UnOp(UnOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs,
			}) => match op {
				Op::Add => todo!(),
				Op::Sub => todo!(),
				Op::Abs => todo!(),
				Op::Mul => todo!(),
				Op::Div => todo!(),
				Op::UDiv => todo!(),
				Op::FAdd => todo!(),
				Op::FSub => todo!(),
				Op::FAbs => todo!(),
				Op::FMul => todo!(),
				Op::FDiv => todo!(),
				Op::And => todo!(),
				Op::Or => todo!(),
				Op::Xor => todo!(),
				Op::Not => todo!(),
				Op::StoreMem => todo!(),
				Op::LoadMem => match lhs {
					Register::Literal(v) => assembler.mov_lit(GP[t], v),
					Register::FloatingPoint(_) => bail!(Error::InvalidIR),
					Register::GeneralPurpose(v) => assembler.mov(GP[t], GP[v]),
					Register::StackPointer => assembler.mov(GP[t], RSP),
					Register::FramePointer => assembler.mov(GP[t], RBP),
					Register::ProgramCounter => bail!(Error::Unsupported),
				},
				Op::Move => todo!(),
				Op::Swap => assembler.xchg(
					GP[t],
					GP[lhs.general_purpose().ok_or(Error::InvalidIR)?],
				),
			},
			Expression::FunctionCall(FunctionCall {
				target,
				scope,
				args,
			}) => {
				// Save registers
				// Load arguments
				// Copy RAX to GP[target]
				// Restore registers
				todo!()
			}
			_ => bail!(Error::InvalidIR),
		}
	}
	match out {
		&BlockEnd::Return(Register::GeneralPurpose(c)) => {
			if GP[c] != RAX {
				assembler.mov(GeneralPurposeRegister::RAX, GP[c]);
			}
			assembler.ret()
		}
		BlockEnd::One(next) => assembler.jmp(next.label(name)?),
		&BlockEnd::Two(Register::GeneralPurpose(c), left, right) => {
			assembler.test(GP[c], GP[c]);
			assembler.jne(left.label(name)?);
			assembler.jmp(right.label(name)?);
		}
		BlockEnd::Two(Register::Literal(c), ..) => todo!(),
		_ => todo!(),
	}
	Ok(())
}
