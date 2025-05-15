use super::semigroup::Semigroup;

pub trait Monoid<T, U>: Mempty + Semigroup<T, U> {
    type Mempty;
    type Mappend;
}
impl<T, U, X> Monoid<T, U> for X
where
    X: Mempty + Semigroup<T, U>,
{
    type Mempty = <X as Mempty>::Out;
    type Mappend = <X as Semigroup<T, U>>::Mappend;
}

pub trait Mempty {
    type Out;
}
