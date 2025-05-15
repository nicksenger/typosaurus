use typenum::{B0, B1};

pub struct True;
pub struct False;

pub trait TruthValue {}
impl TruthValue for True {}
impl TruthValue for False {}

pub trait Or {
    type Out;
}
impl Or for (True, True) {
    type Out = True;
}
impl Or for (True, False) {
    type Out = True;
}
impl Or for (False, True) {
    type Out = True;
}
impl Or for (False, False) {
    type Out = False;
}

pub trait And {
    type Out;
}
impl And for (True, True) {
    type Out = True;
}
impl And for (True, False) {
    type Out = False;
}
impl And for (False, True) {
    type Out = False;
}
impl And for (False, False) {
    type Out = False;
}

pub trait Not {
    type Out;
}
impl Not for True {
    type Out = False;
}
impl Not for False {
    type Out = True;
}

pub trait Bool {
    type Out;
}
impl Bool for B1 {
    type Out = True;
}
impl Bool for B0 {
    type Out = False;
}

pub trait Truthy {}
impl Truthy for True {}

pub trait Falsy {}
impl Falsy for False {}

pub mod monoid {
    use super::*;
    use crate::traits::{monoid::Mempty, semigroup::Semigroup};

    pub struct Either;
    pub struct Both;

    impl<Lhs, Rhs> Semigroup<Lhs, Rhs> for Either
    where
        (Lhs, Rhs): Or,
    {
        type Mappend = <(Lhs, Rhs) as Or>::Out;
    }
    impl Mempty for Either {
        type Out = False;
    }

    impl<Lhs, Rhs> Semigroup<Lhs, Rhs> for Both
    where
        (Lhs, Rhs): And,
    {
        type Mappend = <(Lhs, Rhs) as And>::Out;
    }
    impl Mempty for Both {
        type Out = True;
    }
}
