use crate::traits::{monoid::Mempty, semigroup::Semigroup};
use core::ops::{Add, Mul};

pub use typenum::{Bit, IsEqual, IsGreater, IsLess, Max, Min, NonZero, UInt, UTerm, Unsigned};
pub mod consts {
    pub use typenum::consts::*;
}

pub struct Addition;
pub struct Multiplication;

impl<Lhs, Rhs> Semigroup<Lhs, Rhs> for Addition
where
    Lhs: Add<Rhs>,
{
    type Mappend = <Lhs as Add<Rhs>>::Output;
}
impl Mempty for Addition {
    type Out = consts::U0;
}

impl<Lhs, Rhs> Semigroup<Lhs, Rhs> for Multiplication
where
    Lhs: Mul<Rhs>,
{
    type Mappend = <Lhs as Mul<Rhs>>::Output;
}
impl Mempty for Multiplication {
    type Out = consts::U1;
}
