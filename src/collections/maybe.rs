use core::marker::PhantomData;

use crate::{
    bool::{False, True},
    cmp::IsEqual,
    traits::{
        fold::Foldable,
        functor::{Map, Mapper},
        monoid::{Mempty, Monoid},
        semigroup::{Mappend, Semigroup},
    },
};

use super::{
    list::{self, List},
    Container,
};

pub struct Maybe<T>(PhantomData<T>);
pub type Just<T> = Maybe<List<(T, list::Empty)>>;
pub type Nothing = Maybe<list::Empty>;

impl<T> Container for Maybe<T>
where
    T: Container,
{
    type Content = <T as Container>::Content;
}

impl<T> Mempty for Maybe<T> {
    type Out = Nothing;
}

impl Mappend for (Nothing, Nothing) {
    type Out = Nothing;
}
impl<T> Mappend for (Just<T>, Nothing) {
    type Out = Just<T>;
}
impl<T> Mappend for (Nothing, Just<T>) {
    type Out = Just<T>;
}
impl<T, U> Mappend for (Just<T>, Just<U>)
where
    (T, U): Mappend,
{
    type Out = Maybe<<(T, U) as Mappend>::Out>;
}

impl<T, M> Map<T, M> for (Nothing, M) {
    type Out = Nothing;
}

impl<T, M> Map<T, M> for (Just<T>, M)
where
    M: Mapper<T>,
{
    type Out = Just<<M as Mapper<T>>::Out>;
}

impl<T, M> Foldable<M> for Just<T>
where
    M: Monoid<T, Nothing>,
{
    type Out = <M as Monoid<T, Nothing>>::Mappend;
}
impl<M> Foldable<M> for Nothing {
    type Out = Nothing;
}

pub struct Filter;
impl<Lhs, Rhs> Semigroup<(Lhs, True), Rhs> for Filter
where
    (Rhs, Just<Lhs>): Mappend,
{
    type Mappend = <(Rhs, Just<Lhs>) as Mappend>::Out;
}
impl<Lhs, Rhs> Semigroup<(Lhs, False), Rhs> for Filter {
    type Mappend = Rhs;
}
impl Mempty for Filter {
    type Out = Nothing;
}

impl<T, U> IsEqual for (Just<T>, Just<U>)
where
    (T, U): IsEqual,
{
    type Out = <(T, U) as IsEqual>::Out;
}
impl<T> IsEqual for (Just<T>, Nothing) {
    type Out = False;
}
impl<T> IsEqual for (Nothing, Just<T>) {
    type Out = False;
}
impl IsEqual for (Nothing, Nothing) {
    type Out = True;
}
