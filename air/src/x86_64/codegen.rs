use std::collections::HashSet;

use anyhow::{bail, Result};
use GeneralPurposeRegister::*;

type SmallString = smallstr::SmallString<[u8; 16]>;

use crate::{
	error::Error,
	register_allocation::{
		BinOp, Block, BlockEnd, CodeConstruct, Expression, FunctionCall, Op, Register, UnOp,
	},
	ir::BlockId,
	x86_64::definition::{AssemblyBuilder, GeneralPurposeRegister},
};

// SYSTEMV ABI for x64
// Stack must be 16-byte aligned and grows downwards
// Arguments go in rdi, rsi, rdx, rcx, r8, r9
// Preserve rbx, rsp, rbp, r12, r13, r14, r15
// Don't care about rax, rdi, rsi, rdz, rcx, r8, r9, r10, r11
// Return value in rax (+ higher bits in rdx for 128 bit values)

/// General purpose registers, in order of priority
const GP: [GeneralPurposeRegister; 13] = {
	[
		RDI, RSI, RDX, RCX, R8, R9, RAX, R10, R11, RBX, R12, R13, R14,
	]
};
const PROTECTED_GP: [GeneralPurposeRegister; 5] = [RBX, R12, R13, R14, R15];

pub fn assemble(constructs: &[CodeConstruct]) -> Result<String> {
	let mut assembler = AssemblyBuilder::new();
	for construct in constructs.iter() {
		match construct {
			CodeConstruct::Function {
				name,
				blocks,
				frame_size,
				..
			} => {
				assembler.global(name);
				assembler.label(name.clone());

				let used_gp = find_used_registers(blocks);

				assembler.push(RBP);
				assembler.mov(RBP, RSP);
				for reg in
					PROTECTED_GP.iter().filter(|r| used_gp.contains(r)).copied()
				{
					assembler.push(reg);
				}
				// Add one if not odd to keep stack alignment to 16
				// We start at !16 because ABI says 16 before `call` and `call` pushes return pointer
				let offset = *frame_size * 8;
				if offset != 0 {
					assembler.sub(RSP, LiteralOffset(offset));
				}

				for (idx, block) in blocks.iter().enumerate() {
					assemble_block(
						block,
						idx.into(),
						&mut assembler,
						name,
						&used_gp,
					)?;
				}

				// Assuming single return, remove the ending ret to paste in the register restoration first
				assembler.remove_ret()?;

				if offset != 0 {
					assembler.add(RSP, LiteralOffset(offset));
				}

				for reg in PROTECTED_GP
					.iter()
					.filter(|r| used_gp.contains(r))
					.rev()
					.copied()
				{
					assembler.pop(reg);
				}
				// Replace with a single leave instruction
				assembler.mov(RSP, RBP);
				assembler.pop(RBP);

				assembler.ret();
			}
			CodeConstruct::Variable { .. } => todo!(),
			CodeConstruct::ImmediateExpression { .. } => todo!(),
		}
	}
	let res = format!("{assembler}");
	Ok(res)
}

fn find_used_registers(blocks: &[Block]) -> HashSet<GeneralPurposeRegister> {
	let mut gp = HashSet::new();

	let mut add_to_set = |reg: Register| match reg {
		Register::Literal(_) => {}
		Register::GeneralPurpose(idx) => {
			gp.insert(GP[idx]);
		}
		Register::StackPointer => {
			gp.insert(GeneralPurposeRegister::RSP);
		}
		Register::FramePointer => {
			gp.insert(GeneralPurposeRegister::RBP);
		}
		Register::ProgramCounter => {
			gp.insert(GeneralPurposeRegister::RIP);
		}
	};

	for block in blocks.iter() {
		for line in block.block.iter() {
			match line {
				Expression::UnOp(UnOp { target, lhs, .. }) => {
					add_to_set(*target);
					add_to_set(*lhs);
				}
				Expression::BinOp(BinOp {
					target, lhs, rhs, ..
				}) => {
					add_to_set(*target);
					add_to_set(*lhs);
					add_to_set(*rhs);
				}
				Expression::FunctionCall(FunctionCall { args, .. }) => {
					for reg in 0..(*args) {
						add_to_set(Register::GeneralPurpose(reg));
					}
				}
			}
		}
	}

	gp
}

fn assemble_block(
	Block { block, out }: &Block,
	id: BlockId,
	assembler: &mut AssemblyBuilder,
	name: &str,
	_used_registers: &HashSet<GeneralPurposeRegister>,
) -> Result<()> {
	assembler.label(id.label(name)?);
	for line in block {
		match line {
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(_t),
				op: Op::Div,
				lhs: Register::GeneralPurpose(_l),
				rhs: Register::GeneralPurpose(_r),
			}) => {
				todo!("I hate x86 division so much");
			}
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(_t),
				op: Op::UDiv,
				lhs: Register::GeneralPurpose(_l),
				rhs: Register::GeneralPurpose(_r),
			}) => {
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
					bail!(Error::InvalidIR(line!()))
				}
			},
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) => convert_binop(assembler, op, GP[t], GP[l], GP[r])?,
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::FramePointer,
				rhs: Register::GeneralPurpose(r),
			}) => convert_binop(
				assembler,
				op,
				GP[t],
				GeneralPurposeRegister::RBP,
				GP[r],
			)?,
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::StackPointer,
				rhs: Register::GeneralPurpose(r),
			}) => convert_binop(
				assembler,
				op,
				GP[t],
				GeneralPurposeRegister::RSP,
				GP[r],
			)?,
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::FramePointer,
				rhs: Register::Literal(l),
			}) => convert_binop(
				assembler,
				op,
				GP[t],
				GeneralPurposeRegister::RBP,
				GeneralPurposeRegister::LiteralOffset(l.wrapping_mul(8)),
			)?,
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::StackPointer,
				rhs: Register::Literal(l),
			}) => convert_binop(
				assembler,
				op,
				GP[t],
				GeneralPurposeRegister::RSP,
				GeneralPurposeRegister::LiteralOffset(l.wrapping_mul(8)),
			)?,
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
					Register::GeneralPurpose(v) => assembler.mov(GP[t], GP[v]),
					Register::StackPointer => assembler.mov(GP[t], RSP),
					Register::FramePointer => assembler.mov(GP[t], RBP),
					Register::ProgramCounter => {
						bail!(Error::Unsupported(line!()))
					}
				},
				Op::Move => todo!(),
				Op::Swap => assembler.xchg(
					GP[t],
					GP[lhs.general_purpose()
						.ok_or(Error::InvalidIR(line!()))?],
				),
			},
			Expression::FunctionCall(FunctionCall {
				function_name,
				args,
			}) => {
				// let alignment = (args % 2 == 0) as u64;
				let alignment = 0;
				let offset = (alignment + *args as u64) * 8;

				if offset != 0 {
					assembler.sub(
						RSP,
						GeneralPurposeRegister::LiteralOffset(offset),
					);
				}

				// RAX contains the number of **floating point** arguments passed
				// to a variadic function.
				// I am not supporting variadic functions at the moment, so let's just
				// hard code this to 0
				assembler.mov_lit(RAX, 0);

				assembler.call(function_name.clone());
				if offset != 0 {
					assembler.add(
						RSP,
						GeneralPurposeRegister::LiteralOffset(offset),
					);
				}
			}
			_ => {
				log::trace!("Invalid Line: {line:#?}");
				bail!(Error::InvalidIR(line!()))
			}
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
		BlockEnd::Two(Register::Literal(_c), ..) => todo!(),
		_ => todo!(),
	}
	Ok(())
}

fn convert_binop(
	assembler: &mut AssemblyBuilder,
	op: Op,
	target: GeneralPurposeRegister,
	left: GeneralPurposeRegister,
	right: GeneralPurposeRegister,
) -> Result<()> {
	match op {
		Op::Add => assembler.lea(target, left, right),
		Op::Sub => {
			assembler.mov(target, left);
			assembler.sub(target, right)
		}
		Op::Mul => {
			assembler.mov(target, left);
			assembler.imul(target, right)
		}
		Op::Div => {
			assembler.mov(target, left);
			// assembler.idiv(target, right)?
			todo!()
		}
		Op::UDiv => {
			assembler.mov(target, left);
			// assembler.div(target, right)?
			todo!()
		}
		Op::And => {
			assembler.mov(target, left);
			assembler.and(target, right);
		}
		Op::Or => {
			assembler.mov(target, left);
			assembler.or(target, right);
		}
		Op::Xor => {
			assembler.mov(target, left);
			assembler.xor(target, right)
		}
		Op::LoadMem => {
			assembler.lea(target, right, left);
			assembler.mov_from_ram(target, target);
		}

		Op::StoreMem => {
			assembler.mov_to_index(target, left, right);
		}

		Op::Swap
		| Op::Move
		| Op::Abs
		| Op::Not
		| Op::FAdd
		| Op::FSub
		| Op::FAbs
		| Op::FMul
		| Op::FDiv => {
			bail!(Error::InvalidIR(line!()))
		}
	}
	Ok(())
}
