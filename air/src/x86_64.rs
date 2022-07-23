use anyhow::{bail, Result};
use iced_x86::{
	code_asm::{gpr64::*, AsmRegister64, CodeAssembler, CodeLabel},
	Formatter, IntelFormatter, NasmFormatter,
};
use object::{
	write::{elf::Writer, Object},
	Architecture, BinaryFormat, Endianness,
};

use crate::{
	error::Error,
	register_allocation::{
		BinOp, Block, BlockEnd, CodeConstruct, Expression, FunctionCall, Op, Register, UnOp,
	},
};

// SYSTEMV ABI for x64
// Stack must be 16-byte aligned and grows downwards
// Arguments go in rdi, rsi, rdx, rcx, r8, r9
// Preserve rbx, rsp, rbp, r12, r13, r14, r15
// Don't care about rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
// Return value in rax (+ higher bits in rdx for 128 bit values)

/// General purpose registers, in order of priority
const GP: [AsmRegister64; 14] = [
	rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11, rbx, r12, r13, r14, r15,
];

pub fn assemble(construct: &CodeConstruct) -> Result<Vec<u8>> {
	match construct {
		CodeConstruct::Function { name, blocks } => assemble_function(name, blocks),
		CodeConstruct::Variable { .. } => todo!(),
		CodeConstruct::ImmediateExpression { .. } => todo!(),
	}
}

pub fn build_object(constructs: &[CodeConstruct]) -> Result<Object> {
	let mut buffer = Vec::new();
	let writer = Writer::new(Endianness::Big, true, &mut buffer);

	todo!()
}

fn assemble_function(name: &str, blocks: &[Block]) -> Result<Vec<u8>> {
	let mut assembler = CodeAssembler::new(64)?;
	let mut labels = (0..blocks.len())
		.map(|_| assembler.create_label())
		.collect::<Vec<_>>();

	for (idx, block) in blocks.iter().enumerate() {
		assembler.set_label(&mut labels[idx])?;
		assemble_block(block, &mut assembler, &mut labels)?;
	}

	for instruction in assembler.instructions().iter() {
		eprintln!("{instruction}");
	}

	let res = assembler.assemble(0)?;
	Ok(res)
}

fn assemble_block(
	Block { block, out }: &Block,
	assembler: &mut CodeAssembler,
	labels: &mut [CodeLabel],
) -> Result<()> {
	for line in block {
		match line {
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) if t == l => match op {
				Op::Add => assembler.add(GP[l], GP[r])?,
				Op::Sub => assembler.sub(GP[l], GP[r])?,
				Op::Abs => todo!(),
				Op::Mul => assembler.imul_2(GP[l], GP[r])?,
				Op::Div => {
					// x86 only does division of arg / rdx -> (res: rax, rem: rdx)
					// Therefore we store the old values and restore them afterwards
					// unless they're the target register
					assert_ne!(GP[r], rax);
					assert_ne!(GP[l], rax);
					assert_ne!(GP[r], rdx);
					assert_ne!(GP[l], rdx);

					if GP[t] != rax {
						assembler.push(rax)?;
					}
					if GP[t] != rdx {
						assembler.push(rdx)?;
					}
					assembler.mov(rax, GP[l])?;
					assembler.div(GP[r])?;
					assembler.mov(GP[t], rax)?;
					if GP[t] != rdx {
						assembler.pop(rdx)?;
					}
					if GP[t] != rdx {
						assembler.pop(rax)?;
					}
				}
				Op::UDiv => {
					// x86 only does division of arg / rdx -> (res: rax, rem: rdx)
					// Therefore we store the old values and restore them afterwards
					// unless they're the target register
					assert_ne!(GP[r], rax);
					assert_ne!(GP[l], rax);
					assert_ne!(GP[r], rdx);
					assert_ne!(GP[l], rdx);

					if GP[t] != rax {
						assembler.push(rax)?;
					}
					if GP[t] != rdx {
						assembler.push(rdx)?;
					}
					assembler.mov(rdx, 0u64)?;
					assembler.mov(rax, GP[l])?;
					assembler.div(GP[r])?;
					assembler.mov(GP[t], rax)?;
					if GP[t] != rdx {
						assembler.pop(rdx)?;
					}
					if GP[t] != rdx {
						assembler.pop(rax)?;
					}
				}
				Op::And => assembler.and(GP[t], GP[r])?,
				Op::Or => assembler.or(GP[l], GP[r])?,
				Op::Xor => assembler.xor(GP[l], GP[r])?,
				Op::Not => todo!(),
				Op::StoreMem => todo!(),
				Op::LoadMem => todo!(),
				Op::Move => todo!(),
				Op::FAdd | Op::FSub | Op::FAbs | Op::FMul | Op::FDiv => {
					bail!(Error::InvalidIR)
				}
			},
			&Expression::BinOp(BinOp {
				target: Register::GeneralPurpose(t),
				op,
				lhs: Register::GeneralPurpose(l),
				rhs: Register::GeneralPurpose(r),
			}) => match op {
				Op::Add => assembler.lea(GP[t], GP[l] + GP[r])?,
				Op::Sub => {
					assembler.mov(GP[t], GP[l])?;
					assembler.sub(GP[t], GP[r])?
				}
				Op::Mul => {
					assembler.mov(GP[t], GP[l])?;
					assembler.imul_2(GP[t], GP[r])?
				}
				Op::Div => {
					assembler.mov(GP[t], GP[l])?;
					// assembler.idiv(GP[t], GP[r])?
					todo!()
				}
				Op::UDiv => {
					assembler.mov(GP[t], GP[l])?;
					// assembler.div(GP[t], GP[r])?
					todo!()
				}
				Op::And => {
					assembler.mov(GP[t], GP[l])?;
					assembler.and(GP[t], GP[r])?
				}
				Op::Or => {
					assembler.mov(GP[t], GP[l])?;
					assembler.or(GP[t], GP[r])?
				}
				Op::Xor => {
					assembler.mov(GP[t], GP[l])?;
					assembler.xor(GP[t], GP[r])?
				}
				Op::StoreMem => {
					assembler.mov(GP[t], GP[l])?;
					// assembler.imul(GP[t], GP[r])?
					todo!()
				}
				Op::LoadMem => {
					assembler.mov(GP[t], GP[l])?;
					// assembler.imul(GP[t], GP[r])?
					todo!()
				}
				Op::Move => {
					// Is this really ok?
					assembler.mov(GP[t], GP[l] + GP[r])?
				}

				Op::Abs
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
					Register::Literal(v) => assembler.mov(GP[t], v)?,
					Register::FloatingPoint(_) => bail!(Error::InvalidIR),
					Register::GeneralPurpose(v) => {
						assembler.mov(GP[t], GP[v])?
					}
					Register::StackPointer => assembler.mov(GP[t], rsp)?,
					Register::FramePointer => assembler.mov(GP[t], rbp)?,
					Register::ProgramCounter => bail!(Error::Unsupported),
				},
				Op::Move => todo!(),
			},
			Expression::FunctionCall(FunctionCall {
				target,
				scope,
				args,
			}) => {
				// Save registers
				// Load arguments
				// Copy rax to GP[target]
				// Restore registers
				todo!()
			}
			_ => bail!(Error::InvalidIR),
		}
	}
	match out {
		BlockEnd::Return => assembler.ret()?,
		BlockEnd::One(next) => assembler.jmp(labels[next.usize()])?,
		&BlockEnd::Two(Register::GeneralPurpose(c), left, right) => {
			assembler.test(GP[c], GP[c])?;
			assembler.je(labels[left.usize()])?;
			assembler.jmp(labels[right.usize()])?;
		}
		BlockEnd::Two(Register::Literal(c), ..) => todo!(),
		_ => todo!(),
	}
	Ok(())
}
