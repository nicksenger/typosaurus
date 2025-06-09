use core::ops::Add;
use core::ops::Sub;

use typenum::{Bit, IsEqual, UInt, Unsigned, U0, U1};

use crate::bool::monoid::{Both, Either};
use crate::bool::{And, Bool, False, Not, Or, True};
use crate::collections::Container;
use crate::num::Addition;
use crate::traits::monoid::Monoid;
use crate::traits::{
    fold::Foldable,
    functor::{Map, Mapper},
    monoid::Mempty,
    semigroup::Mappend,
};

#[macro_export]
macro_rules! value_list {
    [$a:expr] => { $crate::collections::value_list::List(($a, $crate::collections::value_list::List(()))) };
    [$a:expr,$($bs:expr),+] => { $crate::collections::value_list::List(($a, $crate::value_list![$($bs),+])) };
}
#[macro_export]
macro_rules! value_list_ty {
    [$a:ty] => { $crate::collections::value_list::List<($a, $crate::collections::value_list::List<()>)> };
    [$a:ty,$($bs:ty),+] => { $crate::collections::value_list::List<($a, $crate::value_list_ty![$($bs),+])> };
}
pub type Append<A, B> = <(A, B) as Mappend>::Out;
pub type Idx<T, N> = <(T, N) as Indexed>::Out;
pub type Head<T> = <T as Container>::Content;
pub type Tail<T> = <T as Tailed>::Tail;
pub type Skip<T, N> = <(T, N) as Skippable>::Out;
pub type Take<T, N> = <(T, N) as Takeable>::Out;

pub trait IsUnique {
    type Out;
}
impl IsUnique for Empty {
    type Out = True;
}
impl<T, U> IsUnique for List<(T, U)>
where
    (U, T): IsContainedBy,
    <(U, T) as IsContainedBy>::Out: Not,
    U: IsUnique,
    (
        <<(U, T) as IsContainedBy>::Out as Not>::Out,
        <U as IsUnique>::Out,
    ): And,
{
    type Out = <(
        <<(U, T) as IsContainedBy>::Out as Not>::Out,
        <U as IsUnique>::Out,
    ) as And>::Out;
}

pub trait IsContainedBy {
    type Out;
}
impl<T> IsContainedBy for (Empty, T) {
    type Out = False;
}
impl<T, U, V> IsContainedBy for (List<(U, V)>, T)
where
    T: IsEqual<U>,
    <T as IsEqual<U>>::Output: Bool,
    (V, T): IsContainedBy,
    (
        <<T as IsEqual<U>>::Output as Bool>::Out,
        <(V, T) as IsContainedBy>::Out,
    ): Or,
{
    type Out = <(
        <<T as IsEqual<U>>::Output as Bool>::Out,
        <(V, T) as IsContainedBy>::Out,
    ) as Or>::Out;
}

pub trait Takeable {
    type Out;
}
impl Takeable for (Empty, U0) {
    type Out = Empty;
}
impl<U, B> Takeable for (Empty, UInt<U, B>)
where
    U: Unsigned,
    B: Bit,
{
    type Out = Empty;
}
impl<H, T> Takeable for (List<(H, T)>, U0) {
    type Out = Empty;
}
impl<H, T, U, B> Takeable for (List<(H, T)>, UInt<U, B>)
where
    U: Unsigned,
    B: Bit,
    UInt<U, B>: Sub<U1>,
    (T, <UInt<U, B> as Sub<U1>>::Output): Takeable,
    (
        List<(H, Empty)>,
        <(T, <UInt<U, B> as Sub<U1>>::Output) as Takeable>::Out,
    ): Mappend,
{
    type Out = <(
        List<(H, Empty)>,
        <(T, <UInt<U, B> as Sub<U1>>::Output) as Takeable>::Out,
    ) as Mappend>::Out;
}

pub trait Skippable {
    type Out;
}
impl<N> Skippable for (Empty, N) {
    type Out = Empty;
}
impl<H, T> Skippable for (List<(H, T)>, U0) {
    type Out = List<(H, T)>;
}
impl<H, T> Skippable for (List<(H, T)>, U1) {
    type Out = T;
}
impl<H, T, U, Ba, Bb> Skippable for (List<(H, T)>, UInt<UInt<U, Ba>, Bb>)
where
    U: Unsigned,
    Ba: Bit,
    Bb: Bit,
    UInt<UInt<U, Ba>, Bb>: Sub<U1>,
    (T, <UInt<UInt<U, Ba>, Bb> as Sub<U1>>::Output): Skippable,
{
    type Out = <(T, <UInt<UInt<U, Ba>, Bb> as Sub<U1>>::Output) as Skippable>::Out;
}

type IntoOnes<T> = <(T, Ones) as Map<<T as Container>::Content, Ones>>::Out;
pub type Len<T> = <IntoOnes<T> as Foldable<Addition>>::Out;
pub type All<T> = <T as Foldable<Both>>::Out;
pub type Any<T> = <T as Foldable<Either>>::Out;

pub trait Indexed {
    type Out;
}
impl<A, B> Indexed for (List<(A, B)>, U0) {
    type Out = A;
}
impl<A, B, T, U> Indexed for (List<(A, B)>, UInt<U, T>)
where
    UInt<U, T>: Sub<U1>,
    (B, <UInt<U, T> as Sub<U1>>::Output): Indexed,
{
    type Out = <(B, <UInt<U, T> as Sub<U1>>::Output) as Indexed>::Out;
}

pub trait Tailed {
    type Tail;
}
impl<A, B> Tailed for List<(A, B)> {
    type Tail = B;
}

pub type Empty = List<()>;
pub struct List<T>(pub T);
impl<T1> List<(T1, Empty)> {
    pub fn tuplify(self) -> T1 {
        self.0 .0
    }
}
macro_rules! tuplify {
    [($t:ident,$h:ident),$(($ts:ident,$hs:ident)),+] => {
        impl<$t,$($ts),+> value_list_ty![$t,$($ts),+] {
            pub fn tuplify(self) -> ($t,$($ts),+) {
                let ($h, tail) = (self.0.0, self.0.1);
                $(
                    let $hs = tail.0.0;
                    #[allow(unused)]
                    let tail = tail.0.1;
                )+
                ($h,$($hs),+)
            }
        }
    }
}
tuplify![(T1, a), (T2, b)];
tuplify![(T1, a), (T2, b), (T3, c)];
tuplify![(T1, a), (T2, b), (T3, c), (T4, d)];
tuplify![(T1, a), (T2, b), (T3, c), (T4, d), (T5, e)];
tuplify![(T1, a), (T2, b), (T3, c), (T4, d), (T5, e), (T6, f)];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j),
    (T11, k)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j),
    (T11, k),
    (T12, l)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j),
    (T11, k),
    (T12, l),
    (T13, m)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j),
    (T11, k),
    (T12, l),
    (T13, m),
    (T14, n)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j),
    (T11, k),
    (T12, l),
    (T13, m),
    (T14, n),
    (T15, o)
];
tuplify![
    (T1, a),
    (T2, b),
    (T3, c),
    (T4, d),
    (T5, e),
    (T6, f),
    (T7, g),
    (T8, h),
    (T9, i),
    (T10, j),
    (T11, k),
    (T12, l),
    (T13, m),
    (T14, n),
    (T15, o),
    (T16, p)
];

impl<T1> List<(T1, Empty)> {
    pub fn into_array(self) -> [T1; 1] {
        [self.0 .0]
    }
}
macro_rules! arrayify {
    [$n:literal,$h:ident,$($hs:ident),+] => {
        impl<T> arrayify_ty!($h,$($hs),+) {
            pub fn into_array(self) -> [T; $n] {
                let ($h, tail) = (self.0.0, self.0.1);
                $(
                    let $hs = tail.0.0;
                    #[allow(unused)]
                    let tail = tail.0.1;
                )+
                [$h,$($hs),+]
            }
        }
    }
}
macro_rules! arrayify_ty {
    [$h:ident] => { List<(T, Empty)> };
    [$h:ident,$($hs:ident),+] => {
        List<(T, arrayify_ty!($($hs),*))>
    }
}
arrayify![2,a,b];
arrayify![3,a,b,c];
arrayify![4,a,b,c,d];
arrayify![5,a,b,c,d,e];
arrayify![6,a,b,c,d,e,f];
arrayify![7,a,b,c,d,e,f,g];
arrayify![8,a,b,c,d,e,f,g,h];
arrayify![9,a,b,c,d,e,f,g,h,i];
arrayify![10,a,b,c,d,e,f,g,h,i,j];
arrayify![11,a,b,c,d,e,f,g,h,i,j,k];
arrayify![12,a,b,c,d,e,f,g,h,i,j,k,l];
arrayify![13,a,b,c,d,e,f,g,h,i,j,k,l,m];
arrayify![14,a,b,c,d,e,f,g,h,i,j,k,l,m,n];
arrayify![15,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o];
arrayify![16,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p];

impl Container for List<()> {
    type Content = ();
}
impl<T, U> Container for List<(T, U)> {
    type Content = T;
}

impl<T> Mempty for List<T> {
    type Out = List<()>;
}

impl Mappend for (List<()>, List<()>) {
    type Out = List<()>;
}
impl<A, B> Mappend for (List<()>, List<(A, B)>) {
    type Out = List<(A, B)>;
}
impl<A, B> Mappend for (List<(A, B)>, List<()>) {
    type Out = List<(A, B)>;
}
impl<A, B, C, D> Mappend for (List<(A, B)>, List<(C, D)>)
where
    (B, List<(C, D)>): Mappend,
{
    type Out = List<(A, <(B, List<(C, D)>) as Mappend>::Out)>;
}

impl<A, B, C, M> Map<A, M> for (List<(A, List<(B, C)>)>, M)
where
    M: Mapper<A> + Mapper<B>,
    (List<(B, C)>, M): Map<B, M>,
{
    type Out = List<(<M as Mapper<A>>::Out, <(List<(B, C)>, M) as Map<B, M>>::Out)>;
}
impl<T, M> Map<T, M> for (List<(T, Empty)>, M)
where
    M: Mapper<T>,
    (Empty, M): Map<(), M>,
{
    type Out = List<(<M as Mapper<T>>::Out, <(Empty, M) as Map<(), M>>::Out)>;
}
impl<M> Map<(), M> for (Empty, M) {
    type Out = Empty;
}

impl<T, U, M> Foldable<M> for List<(T, U)>
where
    U: Foldable<M>,
    M: Monoid<T, <U as Foldable<M>>::Out>,
{
    type Out = <M as Monoid<T, <U as Foldable<M>>::Out>>::Mappend;
}
impl<M> Foldable<M> for Empty
where
    M: Mempty,
{
    type Out = <M as Mempty>::Out;
}

pub struct Ones;
impl<T> Mapper<T> for Ones {
    type Out = U1;
}

pub trait Reversible {
    type Out;
}
impl Reversible for Empty {
    type Out = Empty;
}
impl<T, U> Reversible for List<(T, U)>
where
    U: Reversible,
    (<U as Reversible>::Out, List<(T, Empty)>): Mappend,
{
    type Out = <(<U as Reversible>::Out, List<(T, Empty)>) as Mappend>::Out;
}
pub type Rev<T> = <T as Reversible>::Out;

pub trait Enumerable<N> {
    type Out;
}
impl<N> Enumerable<N> for Empty {
    type Out = Empty;
}
impl<T, U> Enumerable<U0> for List<(T, U)>
where
    U: Enumerable<U1>,
    (
        List<(List<(U0, List<(T, Empty)>)>, Empty)>,
        <U as Enumerable<U1>>::Out,
    ): Mappend,
{
    type Out = <(
        List<(List<(U0, List<(T, Empty)>)>, Empty)>,
        <U as Enumerable<U1>>::Out,
    ) as Mappend>::Out;
}
impl<T, U, A, B> Enumerable<UInt<A, B>> for List<(T, U)>
where
    UInt<A, B>: Add<U1>,
    U: Enumerable<<UInt<A, B> as Add<U1>>::Output>,
    (
        List<(List<(UInt<A, B>, List<(T, Empty)>)>, Empty)>,
        <U as Enumerable<<UInt<A, B> as Add<U1>>::Output>>::Out,
    ): Mappend,
{
    type Out = <(
        List<(List<(UInt<A, B>, List<(T, Empty)>)>, Empty)>,
        <U as Enumerable<<UInt<A, B> as Add<U1>>::Output>>::Out,
    ) as Mappend>::Out;
}
pub type Enumerate<T> = <T as Enumerable<U0>>::Out;

pub trait Zippable {
    type Out;
}
impl Zippable for (Empty, Empty) {
    type Out = Empty;
}
impl<T, U> Zippable for (Empty, List<(T, U)>) {
    type Out = Empty;
}
impl<T, U> Zippable for (List<(T, U)>, Empty) {
    type Out = Empty;
}
impl<A, B, T, U> Zippable for (List<(A, T)>, List<(B, U)>)
where
    (T, U): Zippable,
    (
        List<(List<(A, List<(B, Empty)>)>, Empty)>,
        <(T, U) as Zippable>::Out,
    ): Mappend,
{
    type Out = <(
        List<(List<(A, List<(B, Empty)>)>, Empty)>,
        <(T, U) as Zippable>::Out,
    ) as Mappend>::Out;
}
pub type Zip<T, U> = <(T, U) as Zippable>::Out;
