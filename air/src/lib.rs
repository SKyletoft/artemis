#![warn(clippy::trivially_copy_pass_by_ref)]

pub mod aarch64;
pub mod error;
pub mod register_allocation;
pub mod simplify;
pub mod x86_64;
