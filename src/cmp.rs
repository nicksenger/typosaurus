use core::marker::PhantomData;

use crate::{
    bool::{Bool, Not},
    collections::Container,
    traits::functor::{Map, Mapper},
};

pub trait Equal {}

pub trait IsEqual {
    type Out;
}

impl<T, U> IsEqual for (T, U)
where
    T: typenum::IsEqual<U>,
    <T as typenum::IsEqual<U>>::Output: Bool,
{
    type Out = <<T as typenum::IsEqual<U>>::Output as Bool>::Out;
}

pub trait IsNotEqual {
    type Out;
}

impl<T, U> IsNotEqual for (T, U)
where
    (T, U): IsEqual,
    <(T, U) as IsEqual>::Out: Not,
{
    type Out = <<(T, U) as IsEqual>::Out as Not>::Out;
}

pub struct IsEqualTo<M>(PhantomData<M>);
impl<N, M> Mapper<N> for IsEqualTo<M>
where
    (N, M): IsEqual,
{
    type Out = <(N, M) as IsEqual>::Out;
}
pub type EqualTo<T, N> = <(T, IsEqualTo<N>) as Map<<T as Container>::Content, IsEqualTo<N>>>::Out;
