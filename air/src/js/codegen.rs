use std::collections::HashSet;

use rayon::prelude::*;

use crate::simplify::{
	Block, BlockEnd, BlockId, PhiEdge, PhiNode, SSAConstruct, SimpleBinOp, SimpleExpression,
	SimpleFunctionCall, SimpleOp, SimpleUnOp, Source,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

pub fn assemble(ssa: &[SSAConstruct]) -> String {
	"\"use strict\";\n\n".to_owned()
		+ &ssa.iter()
			.map(assemble_function)
			.collect::<Vec<_>>()
			.join("\n\n") + "\n\nconsole.log(f(0) + \", \" + f(1));"
}

fn format_source(src: Source) -> String {
	match src {
		Source::Register(r) => format!("regs.r{}", usize::from(r)),
		Source::Value(v) => format!("{v}"),
	}
}

fn assemble_function(ssa: &SSAConstruct) -> String {
	match ssa {
		SSAConstruct::Function { name, args, blocks } => {
			let args_str = (0..*args)
				.map(|x| format!("r{x}"))
				.collect::<Vec<_>>()
				.join(", ");
			let args_init = (0..*args)
				.map(|x| format!("\t\tr{x}: r{x}"))
				.collect::<Vec<_>>()
				.join(",\n");
			let code_str = blocks
				.iter()
				.map(assemble_block)
				.enumerate()
				.map(|(idx, code)| format!("case {idx}:\n{code}break;\n"))
				.collect::<String>();

			format!("const {name} = ({args_str}) => {{\n\
					\tlet from = 0;\n\
					\tlet at = 0;\n\
					\tconst regs = {{\n\
						{args_init}\n\
					\t}};\n\
				\n\
					\twhile (true) {{\n\
						\t\tswitch (at) {{\n\
							{code_str}\n\
						\t\t}}\n\
					\t}}\n\
				}};")
		}
		SSAConstruct::Variable { name, value } => todo!(),
		SSAConstruct::ImmediateExpression { name, value } => todo!(),
	}
}

fn assemble_block(block: &Block) -> String {
	let Block { intro, block, out } = block;

	let phi = assemble_phi_nodes(intro);
	let code = assemble_code(block);
	let next = assemble_out(*out);

	format!("{phi}{code}{next}")
}

fn assemble_phi_nodes(nodes: &[PhiNode]) -> String {
	nodes.iter()
		.flat_map(|phi_node| [phi_node.value[0].from, phi_node.value[1].from].into_iter())
		.collect::<HashSet<_>>() // Deduplication
		.into_iter()
		.map(|outer_from| {
			nodes.iter()
				.flat_map(|PhiNode { target, value }| {
					value.iter().filter_map(|phi_edge| {
						(phi_edge.from == outer_from)
							.then_some((*target, phi_edge.value))
					})
				})
				.fold(
					format!("if (from === {}) {{\n", usize::from(outer_from)),
					|acc, (target, value)| {
						let target_fmt = usize::from(target);
						let value_fmt = format_source(value);
						acc + &format!(
							"\tregs.r{target_fmt} = {value_fmt};\n",
						)
					},
				) + "} else "
		})
		.collect::<String>()
		+ "{}\n"
}

fn assemble_code(code: &[SimpleExpression]) -> String {
	code.iter().map(assemble_expression).collect()
}

fn assemble_expression(expr: &SimpleExpression) -> String {
	match expr {
		SimpleExpression::UnOp(SimpleUnOp { target, op, lhs }) => todo!(),
		SimpleExpression::BinOp(SimpleBinOp {
			target,
			op,
			lhs,
			rhs,
		}) => {
			let tar = usize::from(*target);
			let left = format_source(*lhs);
			let right = format_source(*rhs);
			match op {
				SimpleOp::Add => format!("regs.r{tar} = ({left} + {right}) | 0;\n"),
				SimpleOp::Sub => format!("regs.r{tar} = ({left} - {right}) | 0;\n"),
				SimpleOp::Mul => format!("regs.r{tar} = ({left} * {right}) | 0;\n"),
				SimpleOp::Div => format!("regs.r{tar} = ({left} / {right}) | 0;\n"),
				SimpleOp::UDiv => {
					format!("regs.r{tar} = ({left} / {right}) | 0;\n")
				}
				SimpleOp::FAdd => format!("regs.r{tar} = {left} + {right};\n"),
				SimpleOp::FSub => format!("regs.r{tar} = {left} - {right};\n"),
				SimpleOp::FMul => format!("regs.r{tar} = {left} * {right};\n"),
				SimpleOp::FDiv => format!("regs.r{tar} = {left} / {right};\n"),
				SimpleOp::And => format!("regs.r{tar} = {left} & {right};\n"),
				SimpleOp::Or => format!("regs.r{tar} = {left} | {right};\n"),
				SimpleOp::Xor => format!("regs.r{tar} = {left} ^ {right};\n"),
				SimpleOp::Abs | SimpleOp::FAbs | SimpleOp::Not => {
					panic!("Invalid IR")
				}
			}
		}
		SimpleExpression::FunctionCall(SimpleFunctionCall {
			target,
			function,
			args,
		}) => todo!(),
	}
}

fn assemble_out(out: BlockEnd) -> String {
	match out {
		BlockEnd::One(next) => format!("from = at;\nat = {};\n", usize::from(next)),
		BlockEnd::Two(cond, then, else_then) => {
			let cond_fmt = format_source(cond);
			let then_fmt = usize::from(then);
			let else_then_fmt = usize::from(else_then);
			format!("from = at;\n\
				if ({cond_fmt} !== 0) {{\n\
				\tat = {then_fmt};\n\
				}} else {{\n\
				\tat = {else_then_fmt};\n\
				}}\n")
		}
		BlockEnd::Return(val) => {
			let val_fmt = format_source(val);
			format!("return {val_fmt};\n")
		}
	}
}