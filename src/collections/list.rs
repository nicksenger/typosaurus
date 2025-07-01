use core::ops::Add;
use core::{marker::PhantomData, ops::Sub};

use typenum::{Bit, IsEqual, U0, U1, UInt, Unsigned};

use crate::bool::monoid::{Both, Either};
use crate::bool::{And, Bool, False, Not, Or, True};
use crate::collections::Container;
use crate::num::Addition;
use crate::traits::applicative;
use crate::traits::monoid::Monoid;
use crate::traits::semigroup::Semigroup;
use crate::traits::{
    fold::Foldable,
    functor::{Map, Mapper},
    monoid::Mempty,
    semigroup::Mappend,
};

use super::tuple;

#[macro_export]
macro_rules! list {
    [$a:ty] => { $crate::collections::list::List<($a, $crate::collections::list::List<()>)> };
    [$a:ty,$($bs:ty),+] => { $crate::collections::list::List<($a, $crate::list![$($bs),+])> };
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

pub type IntoOnes<T> = <(T, Ones) as Map<<T as Container>::Content, Ones>>::Out;
pub type Len<T> = <IntoOnes<T> as Foldable<Addition>>::Out;
pub type All<T> = <T as Foldable<Both>>::Out;
pub type Any<T> = <T as Foldable<Either>>::Out;
pub type WithNotEquals<T, U> =
    <(T, WithNotEqualTo<U>) as Map<<T as Container>::Content, WithNotEqualTo<U>>>::Out;
pub type WithEquals<T, U> =
    <(T, WithEqualTo<U>) as Map<<T as Container>::Content, WithEqualTo<U>>>::Out;
pub type Exclude<T, U> = <WithNotEquals<T, U> as Foldable<Filter>>::Out;
pub type Only<T, U> = <WithEquals<T, U> as Foldable<Filter>>::Out;
pub type UntupleLeft<T> = <(T, tuple::Left) as Map<<T as Container>::Content, tuple::Left>>::Out;
pub type UntupleRight<T> = <(T, tuple::Right) as Map<<T as Container>::Content, tuple::Right>>::Out;

pub struct WithEqualTo<U>(PhantomData<U>);
impl<T, U> Mapper<T> for WithEqualTo<U>
where
    (T, U): crate::cmp::IsEqual,
{
    type Out = (T, <(T, U) as crate::cmp::IsEqual>::Out);
}
pub struct WithNotEqualTo<U>(PhantomData<U>);
impl<T, U> Mapper<T> for WithNotEqualTo<U>
where
    (T, U): crate::cmp::IsNotEqual,
{
    type Out = (T, <(T, U) as crate::cmp::IsNotEqual>::Out);
}

pub struct Filter;
impl<Lhs, Rhs> Semigroup<(Lhs, True), Rhs> for Filter
where
    (Rhs, List<(Lhs, Empty)>): Mappend,
{
    type Mappend = <(Rhs, List<(Lhs, Empty)>) as Mappend>::Out;
}
impl<Lhs, Rhs> Semigroup<(Lhs, False), Rhs> for Filter {
    type Mappend = Rhs;
}
impl Mempty for Filter {
    type Out = Empty;
}

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
pub struct List<T>(PhantomData<T>);
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

impl<T> applicative::Pure<T> for T {
    type Out = List<(T, Empty)>;
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

pub trait ZipTuple {
    type Out;
}
impl ZipTuple for (Empty, Empty) {
    type Out = Empty;
}
impl<T, U> ZipTuple for (Empty, List<(T, U)>) {
    type Out = Empty;
}
impl<T, U> ZipTuple for (List<(T, U)>, Empty) {
    type Out = Empty;
}
impl<A, B, T, U> ZipTuple for (List<(A, T)>, List<(B, U)>)
where
    (T, U): ZipTuple,
    (List<((A, B), Empty)>, <(T, U) as ZipTuple>::Out): Mappend,
{
    type Out = <(List<((A, B), Empty)>, <(T, U) as ZipTuple>::Out) as Mappend>::Out;
}

#[cfg(test)]
mod test {
    use crate::dinosaurs::*;

    use super::*;

    use typenum::{U2, U3, U4, U5, U10, assert_type_eq};

    #[test]
    #[allow(unused)]
    fn mappend() {
        type Left = <(List<(U1, Empty)>, Empty) as Mappend>::Out;
        type Right = <(Empty, List<(U0, Empty)>) as Mappend>::Out;
        type Cat2 = <(Left, Right) as Mappend>::Out;
        type HeadCat2 = Head<Cat2>;
        assert_type_eq!(HeadCat2, U1);
        type Last2 = <<Cat2 as Tailed>::Tail as Container>::Content;
        assert_type_eq!(Last2, U0);

        type Cat3 = <(List<(U2, Empty)>, Cat2) as Mappend>::Out;
        assert_type_eq!(<Cat3 as Container>::Content, U2);
        type Mid3 = <<Cat3 as Tailed>::Tail as Container>::Content;
        assert_type_eq!(Mid3, U1);
        type Last3 = <<<Cat3 as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(Last3, U0);
        type Merged = <(Cat3, Cat2) as Mappend>::Out;
        type M0 = <Merged as Container>::Content;
        assert_type_eq!(M0, U2);
        type M1 = <<Merged as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M1, U1);
        type M2 = <<<Merged as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M2, U0);
        type M3 =
            <<<<Merged as Tailed>::Tail as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M3, U1);
        type M4 = <<<<<Merged as Tailed>::Tail as Tailed>::Tail as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M4, U0);
    }

    #[test]
    #[allow(unused)]
    fn map() {
        type Dinos = list![Pterodactyl, Velociraptor, Stegosaurus];
        type Mapped = <(Dinos, Ones) as Map<<Dinos as Container>::Content, Ones>>::Out;
        type First = Idx<Mapped, U0>;
        type Second = Idx<Mapped, U1>;
        type Third = Idx<Mapped, U2>;
        assert_type_eq!(First, U1);
        assert_type_eq!(Second, U1);
        assert_type_eq!(Third, U1);
    }

    #[test]
    #[allow(unused)]
    fn fold() {
        type AList = list![U1, U1];
        type Folded = <AList as Foldable<Addition>>::Out;
        assert_type_eq!(Folded, U2);

        type AnotherList = list![U1, U2, U3, U4];
        type AnotherFold = <AnotherList as Foldable<Addition>>::Out;
        assert_type_eq!(AnotherFold, U10);
    }

    #[test]
    #[allow(unused)]
    fn list_macro() {
        type Both = list![U1, U0];
        type Head2 = <Both as Container>::Content;
        assert_type_eq!(Head2, U1);
        type Last2 = <<Both as Tailed>::Tail as Container>::Content;
        assert_type_eq!(Last2, U0);

        type Three = list![U2, U1, U0];
        type Head3 = <Three as Container>::Content;
        assert_type_eq!(Head3, U2);
        type Mid3 = <<Three as Tailed>::Tail as Container>::Content;
        assert_type_eq!(Mid3, U1);

        type Last3 = <<<Three as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(Last3, U0);

        type Moar = list![U2, U1, U0, U1, U0];
        type M0 = <Moar as Container>::Content;
        assert_type_eq!(M0, U2);
        type M1 = <<Moar as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M1, U1);
        type M2 = <<<Moar as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M2, U0);
        type M3 =
            <<<<Moar as Tailed>::Tail as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M3, U1);
        type M4 = <<<<<Moar as Tailed>::Tail as Tailed>::Tail as Tailed>::Tail as Tailed>::Tail as Container>::Content;
        assert_type_eq!(M4, U0);
    }

    #[test]
    #[allow(unused)]
    fn head() {
        type Dinos = list![Stegosaurus, Triceratops, Diplodocus];
        type H = Head<Dinos>;
        assert_type_eq!(H, Stegosaurus);
    }

    #[test]
    #[allow(unused)]
    fn tail() {
        type Dinos = list![
            DaspletosaurusTorosus,
            NanotyranosaurusLancensis,
            TeratophoneusCurriei
        ];
        type T = Head<Tail<Dinos>>;
        assert_type_eq!(T, NanotyranosaurusLancensis);
    }

    #[test]
    #[allow(unused)]
    fn idx() {
        type Dinos = list![Microraptor, Oviraptor, Velociraptor, Herrerasaurus];
        type A = Idx<Dinos, U0>;
        type B = Idx<Dinos, U1>;
        type C = Idx<Dinos, U2>;
        type D = Idx<Dinos, U3>;

        assert_type_eq!(A, Microraptor);
        assert_type_eq!(B, Oviraptor);
        assert_type_eq!(C, Velociraptor);
        assert_type_eq!(D, Herrerasaurus);
    }

    #[test]
    #[allow(unused)]
    fn append() {
        type HugeDinos = list![Megalosaurus, Gigantosaurus];
        type Raptors = list![Velociraptor, Oviraptor, Microraptor];
        type MergedFw = Append<Raptors, HugeDinos>;
        type A1 = Idx<MergedFw, U0>;
        type B1 = Idx<MergedFw, U1>;
        type C1 = Idx<MergedFw, U2>;
        type D1 = Idx<MergedFw, U3>;
        type E1 = Idx<MergedFw, U4>;
        assert_type_eq!(A1, Velociraptor);
        assert_type_eq!(B1, Oviraptor);
        assert_type_eq!(C1, Microraptor);
        assert_type_eq!(D1, Megalosaurus);
        assert_type_eq!(E1, Gigantosaurus);

        type MergedRev = Append<HugeDinos, Raptors>;
        type A2 = Idx<MergedRev, U0>;
        type B2 = Idx<MergedRev, U1>;
        type C2 = Idx<MergedRev, U2>;
        type D2 = Idx<MergedRev, U3>;
        type E2 = Idx<MergedRev, U4>;
        assert_type_eq!(A2, Megalosaurus);
        assert_type_eq!(B2, Gigantosaurus);
        assert_type_eq!(C2, Velociraptor);
        assert_type_eq!(D2, Oviraptor);
        assert_type_eq!(E2, Microraptor);
    }

    #[test]
    #[allow(unused)]
    fn len() {
        type HugeDinos = list![Megalosaurus, Gigantosaurus];
        type Raptors = list![Velociraptor, Oviraptor, Microraptor];
        type NumHuge = Len<HugeDinos>;
        type NumRaptors = Len<Raptors>;
        assert_type_eq!(NumHuge, U2);
        assert_type_eq!(NumRaptors, U3);

        type Dinos = Append<HugeDinos, Raptors>;
        type NumDinos = Len<Dinos>;
        assert_type_eq!(NumDinos, U5);
    }

    #[test]
    #[allow(unused)]
    fn rev() {
        type Dinos = list![Stegosaurus, Triceratops, Pachycephalosaurus];
        type RevList = Rev<Dinos>;
        assert_type_eq!(Idx<RevList, U0>, Pachycephalosaurus);
        assert_type_eq!(Idx<RevList, U1>, Triceratops);
        assert_type_eq!(Idx<RevList, U2>, Stegosaurus);
        assert_type_eq!(RevList, list![Pachycephalosaurus, Triceratops, Stegosaurus]);
    }

    #[test]
    #[allow(unused)]
    fn enumerate() {
        type LongNeckDinos = list![Brachiosaurus, Brontosaurus, Diplodocus];
        type Enumerated = Enumerate<LongNeckDinos>;
        assert_type_eq!(Idx<Enumerated, U0>, list![U0, Brachiosaurus]);
        assert_type_eq!(Idx<Enumerated, U1>, list![U1, Brontosaurus]);
        assert_type_eq!(Idx<Enumerated, U2>, list![U2, Diplodocus]);
    }

    #[test]
    #[allow(unused)]
    fn zip() {
        type LongNecks = list![Brachiosaurus, Brontosaurus, Diplodocus];
        type Raptors = list![Microraptor, Oviraptor, Velociraptor];
        type Zipped = Zip<LongNecks, Raptors>;
        assert_type_eq!(Idx<Zipped, U0>, list![Brachiosaurus, Microraptor]);
        assert_type_eq!(Idx<Zipped, U1>, list![Brontosaurus, Oviraptor]);
        assert_type_eq!(Idx<Zipped, U2>, list![Diplodocus, Velociraptor]);
    }

    #[test]
    #[allow(unused)]
    fn skip() {
        type Tyranosaurs = list![
            TyranosaurusRex,
            NanotyranosaurusLancensis,
            DaspletosaurusTorosus,
            NanuqsaurusHoglundi,
            TeratophoneusCurriei
        ];
        assert_type_eq!(Skip<Tyranosaurs, U0>, Tyranosaurs);
        assert_type_eq!(Skip<Tyranosaurs, U1>, list![NanotyranosaurusLancensis, DaspletosaurusTorosus, NanuqsaurusHoglundi, TeratophoneusCurriei]);
        assert_type_eq!(Skip<Tyranosaurs, U2>, list![DaspletosaurusTorosus, NanuqsaurusHoglundi, TeratophoneusCurriei]);
        assert_type_eq!(Skip<Tyranosaurs, U3>, list![NanuqsaurusHoglundi, TeratophoneusCurriei]);
        assert_type_eq!(Skip<Tyranosaurs, U4>, list![TeratophoneusCurriei]);
        assert_type_eq!(Skip<Tyranosaurs, U5>, Empty);
    }

    #[test]
    #[allow(unused)]
    fn take() {
        type Tyranosaurs = list![
            TyranosaurusRex,
            NanotyranosaurusLancensis,
            DaspletosaurusTorosus,
            NanuqsaurusHoglundi,
            TeratophoneusCurriei
        ];
        assert_type_eq!(Take<Tyranosaurs, U0>, Empty);
        assert_type_eq!(Take<Tyranosaurs, U1>, list![TyranosaurusRex]);
        assert_type_eq!(Take<Tyranosaurs, U2>, list![TyranosaurusRex, NanotyranosaurusLancensis]);
        assert_type_eq!(Take<Tyranosaurs, U3>, list![TyranosaurusRex, NanotyranosaurusLancensis, DaspletosaurusTorosus]);
        assert_type_eq!(Take<Tyranosaurs, U4>, list![TyranosaurusRex, NanotyranosaurusLancensis, DaspletosaurusTorosus, NanuqsaurusHoglundi]);
        assert_type_eq!(Take<Tyranosaurs, U5>, Tyranosaurs);
    }

    #[test]
    #[allow(unused)]
    fn is_contained_by() {
        type Numbers = list![U0, U1, U2];
        assert_type_eq!(<(Numbers, U1) as IsContainedBy>::Out, True);
        assert_type_eq!(<(Numbers, U4) as IsContainedBy>::Out, False);
    }

    #[test]
    #[allow(unused)]
    fn is_unique() {
        type A = list![U0, U1, U2];
        type B = list![U0, U0, U1, U2];
        assert_type_eq!(<A as IsUnique>::Out, True);
        assert_type_eq!(<B as IsUnique>::Out, False);
    }
}
