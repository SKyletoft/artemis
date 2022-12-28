use std::{cmp::Ordering, collections::HashMap};

use anyhow::Result;

use crate::ir::{
	Block, BlockEnd, PhiEdge, PhiNode, SSAConstruct, SimpleBinOp, SimpleExpression,
	SimpleFunctionCall, SimpleUnOp, Source,
};

type SmallString = smallstr::SmallString<[u8; 16]>;

#[deprecated]
pub fn has_been_used_for_the_last_time(
	s: &Source,
	(r_block, r_line): (usize, usize),
	last_use_of_register: &HashMap<Source, (usize, usize)>,
	debug: bool,
) -> bool {
	if debug {
		dbg!(last_use_of_register, s, (r_block, r_line));
	}

	last_use_of_register
		.get(s)
		.map(|&(l_block, l_line)| match r_block.cmp(&l_block) {
			Ordering::Less => false,
			Ordering::Equal => l_line < r_line,
			Ordering::Greater => true,
		})
		.unwrap_or(true)
}

/// Counts the amount of lines until the last use of a Source within this function
// TODO: Needs memoisation
// TODO: Doesn't work at all for loops
pub fn lines_till_last_use(
	s: &Source,
	scope: &[Block],
	(block_idx, line_idx): (usize, usize),
) -> Option<usize> {
	let block = &scope[block_idx];
	let in_this_block = block
		.intro
		.len()
		.saturating_add(block.block.len())
		.saturating_sub(line_idx);

	// If it's the condition of this block we don't need to search through every line
	match &block.out {
		BlockEnd::Two(condition, left, right) if condition == s => {
			let left = usize::from(*left);
			let right = usize::from(*right);

			assert!(scope[left].intro.is_empty());
			assert!(scope[right].intro.is_empty());

			let steps_in_left_block = lines_till_last_use(s, scope, (left, 0));
			let steps_in_right_block = lines_till_last_use(s, scope, (right, 0));

			return match (steps_in_left_block, steps_in_right_block) {
				(Some(l), Some(r)) => Some(l.max(r) + in_this_block),
				(Some(n), None) | (None, Some(n)) => Some(n + in_this_block),
				(None, None) => Some(in_this_block),
			};
		}
		_ => (),
	}

	// Count steps until the next use
	for (steps, line) in block
		.block
		.iter()
		.skip(line_idx.saturating_add(1))
		.enumerate()
	{
		let contains_self = match line {
			SimpleExpression::BinOp(SimpleBinOp { lhs, rhs, .. }) => {
				lhs == s || rhs == s
			}
			SimpleExpression::UnOp(SimpleUnOp { lhs, .. }) => lhs == s,
			SimpleExpression::FunctionCall(SimpleFunctionCall { args, .. }) => {
				args.iter().any(|arg| arg == s)
			}
		};
		if contains_self {
			// Then recurse so we don't have issues with several uses within a block
			let steps_so_far = steps + lines_till_last_use(
				s,
				scope,
				(block_idx, line_idx + steps + 1),
			)
			.unwrap_or(0);
			return Some(steps_so_far);
		}
	}

	let res = match block.out {
		BlockEnd::Return(_) => None,
		BlockEnd::One(next) => {
			let steps_in_intro = scope[usize::from(next)].intro.iter().rposition(
				|PhiNode {
				         value: [PhiEdge { value: l, .. }, PhiEdge { value: r, .. }],
				         ..
				 }| l == s || r == s,
			);

			let steps_in_next_block = lines_till_last_use(s, scope, (next.into(), 0));
			steps_in_next_block
				.or(steps_in_intro)
				.map(|n| n + in_this_block)
		}
		BlockEnd::Two(_, left, right) => {
			// Not actually an invariant, just a todo
			assert!(scope[usize::from(left)].intro.is_empty());
			assert!(scope[usize::from(right)].intro.is_empty());

			let steps_in_left_block = lines_till_last_use(s, scope, (left.into(), 0));
			let steps_in_right_block = lines_till_last_use(s, scope, (right.into(), 0));

			let max = match (steps_in_left_block, steps_in_right_block) {
				(Some(l), Some(r)) => Some(l.max(r)),
				(Some(n), None) | (None, Some(n)) => Some(n),
				(None, None) => None,
			};
			max.map(|v| v + in_this_block)
		}
	};

	res
}

pub fn validate_ir(construct: &SSAConstruct) -> Result<()> {
	todo!()
}
