#![no_std]

pub mod bool;
pub mod cmp;
pub mod collections;
pub mod num;
pub mod traits;

pub use typenum::assert_type_eq;

#[cfg(test)]
#[allow(unused)]
pub(crate) mod dinosaurs;
