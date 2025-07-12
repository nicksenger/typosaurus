use core::marker::PhantomData;

use crate::{
    bool::{monoid::Either, False, Falsy, Not, True, Truthy},
    cmp::{EqualTo, IsEqual, IsEqualTo},
    num::Addition,
    traits::{
        fold::Foldable,
        functor::{Map, Mapper},
        semigroup::Mappend,
    },
};

use super::{
    list::{self, Any, Exclude, Filter, Interleave, IntoOnes, List, Ones, WithNotEqualTo},
    Container,
};

pub struct Set<T>(PhantomData<T>);
pub type Empty = Set<list::Empty>;

pub trait IntoList {
    type Out;
}
impl IntoList for Empty {
    type Out = list::Empty;
}
impl<T, U> IntoList for Set<List<(T, U)>> {
    type Out = List<(T, U)>;
}

impl<T> Container for Set<T>
where
    T: Container,
{
    type Content = <T as Container>::Content;
}

pub trait IsEmpty {
    type Out;
}
impl IsEmpty for Set<list::Empty> {
    type Out = True;
}
impl<T, U> IsEmpty for Set<List<(T, U)>> {
    type Out = False;
}

impl<T, M> Map<<T as Container>::Content, M> for (Set<T>, M)
where
    T: Container,
    (T, M): Map<<T as Container>::Content, M>,
{
    type Out = Set<<(T, M) as Map<<T as Container>::Content, M>>::Out>;
}

pub trait Contains {
    type Out;
}
impl<T, U> Contains for (Set<T>, U)
where
    T: Container,
    (T, IsEqualTo<U>): Map<<T as Container>::Content, IsEqualTo<U>>,
    <(T, IsEqualTo<U>) as Map<<T as Container>::Content, IsEqualTo<U>>>::Out: Foldable<Either>,
{
    type Out = Any<EqualTo<T, U>>;
}

pub trait Insert {
    type Out;
}
impl<T, U> Insert for (Set<T>, U)
where
    (Set<T>, U): Contains,
    <(Set<T>, U) as Contains>::Out: Falsy,
    (T, List<(U, list::Empty)>): Interleave,
{
    type Out = Set<<(T, List<(U, list::Empty)>) as Interleave>::Out>;
}

pub trait Remove {
    type Out;
}
impl<T, U> Remove for (Set<T>, U)
where
    (Set<T>, U): Contains,
    <(Set<T>, U) as Contains>::Out: Truthy,
    T: Container,
    (T, WithNotEqualTo<U>): Map<<T as Container>::Content, WithNotEqualTo<U>>,
    <(T, WithNotEqualTo<U>) as Map<<T as Container>::Content, WithNotEqualTo<U>>>::Out:
        Foldable<Filter>,
{
    type Out = Set<Exclude<T, U>>;
}

pub struct WithContainedIn<U>(PhantomData<U>);
impl<T, U> Mapper<T> for WithContainedIn<U>
where
    (U, T): Contains,
{
    type Out = (T, <(U, T) as Contains>::Out);
}
pub type WithContained<T, U> =
    <(T, WithContainedIn<U>) as Map<<T as Container>::Content, WithContainedIn<U>>>::Out;

pub struct WithNotContainedIn<U>(PhantomData<U>);
impl<T, U> Mapper<T> for WithNotContainedIn<U>
where
    (U, T): Contains,
    <(U, T) as Contains>::Out: Not,
{
    type Out = (T, <<(U, T) as Contains>::Out as Not>::Out);
}
pub type WithNotContained<T, U> =
    <(T, WithNotContainedIn<U>) as Map<<T as Container>::Content, WithNotContainedIn<U>>>::Out;

pub trait Intersection {
    type Out;
}
impl<T, U> Intersection for (Set<T>, Set<U>)
where
    T: Container,
    (T, WithContainedIn<Set<U>>): Map<<T as Container>::Content, WithContainedIn<Set<U>>>,
    WithContained<T, Set<U>>: Foldable<Filter>,
{
    type Out = Set<<WithContained<T, Set<U>> as Foldable<Filter>>::Out>;
}

impl<T, U> Mappend for (Set<T>, Set<U>)
where
    (Set<T>, Set<U>): Union,
{
    type Out = <(Set<T>, Set<U>) as Union>::Out;
}

pub trait Union {
    type Out;
}
impl<T, U> Union for (Set<T>, Set<U>)
where
    T: Container,
    (T, WithNotContainedIn<Set<U>>): Map<<T as Container>::Content, WithNotContainedIn<Set<U>>>,
    WithNotContained<T, Set<U>>: Foldable<Filter>,
    (<WithNotContained<T, Set<U>> as Foldable<Filter>>::Out, U): Interleave,
{
    type Out =
        Set<<(<WithNotContained<T, Set<U>> as Foldable<Filter>>::Out, U) as Interleave>::Out>;
}

pub trait Difference {
    type Out;
}
impl<T, U> Difference for (Set<T>, Set<U>)
where
    T: Container,
    (T, WithNotContainedIn<Set<U>>): Map<<T as Container>::Content, WithNotContainedIn<Set<U>>>,
    WithNotContained<T, Set<U>>: Foldable<Filter>,
{
    type Out = Set<<WithNotContained<T, Set<U>> as Foldable<Filter>>::Out>;
}

impl<T, U> IsEqual for (Set<T>, Set<U>)
where
    (Set<T>, Set<U>): Difference,
    <(Set<T>, Set<U>) as Difference>::Out: IsEmpty,
{
    type Out = <<(Set<T>, Set<U>) as Difference>::Out as IsEmpty>::Out;
}

pub trait Size {
    type Out;
}
impl<T> Size for Set<T>
where
    T: Container,
    (T, Ones): Map<<T as Container>::Content, Ones>,
    IntoOnes<T>: Foldable<Addition>,
{
    type Out = <IntoOnes<T> as Foldable<Addition>>::Out;
}

#[macro_export]
macro_rules! set {
    [$(,)?] => {
        $crate::collections::set::Set<$crate::collections::list::Empty>
    };
    [$t:ty$(,)?] => {
        $crate::collections::set::Set<$crate::list![$t]>
    };
    [$t:ty,$($ts:ty),+] => {
        <($crate::set![$($ts),+], $t) as $crate::collections::set::Insert>::Out
    };
}

#[macro_export]
macro_rules! elements {
    [$(,)?] => {};
    [$t:ty$(,)?] => {
        impl $crate::cmp::Equality<$t> for $t {
            type Out = $crate::bool::True;
        }
    };
    [$t1:ty,$($ts:ty),+$(,)?] => {
        impl $crate::cmp::Equality<$t1> for $t1 {
            type Out = $crate::bool::True;
        }

        $(
            impl $crate::cmp::Equality<$ts> for $t1 {
                type Out = $crate::bool::False;
            }
            impl $crate::cmp::Equality<$t1> for $ts {
                type Out = $crate::bool::False;
            }
        )+

        $crate::elements![$($ts),+];
    };
}

#[macro_export]
macro_rules! merge_sets {
    [$(,)?] => {
      $crate::collections::set::Empty
    };
    [$s:ty$(,)?] => {
        $s
    };
    [$s:ty,$($ss:ty),+$(,)?] => {
        <($s, $crate::merge_sets![$($ss),+]) as $crate::collections::set::Union>::Out
    };
}

#[allow(unused)]
#[cfg(test)]
mod test {
    use super::*;
    use crate::bool::{False, True};
    use crate::dinosaurs::*;
    use crate::num::consts::{U1, U2, U3, U4};
    use crate::{assert_type_eq, elements, set};

    elements![
        Iguanodon,
        TyranosaurusRex,
        Pterodactyl,
        Velociraptor,
        Oviraptor,
        Compsognathus,
        Pachycephalosaurus,
        Stegosaurus,
        Triceratops,
        Brachiosaurus,
        Gallimimus,
        Brontosaurus,
        Diplodocus,
        Spinosaurus
    ];

    #[test]
    fn set_macro() {
        type MySet = set![U1, U2, U3];
        assert_type_eq!(<(MySet, U1) as Contains>::Out, True);
        assert_type_eq!(<(MySet, U2) as Contains>::Out, True);
        assert_type_eq!(<(MySet, U3) as Contains>::Out, True);
        assert_type_eq!(<(MySet, U4) as Contains>::Out, False);
    }

    #[allow(non_local_definitions)]
    #[test]
    fn elements() {
        type JurassicPark = set![Velociraptor, TyranosaurusRex];
        assert_type_eq!(<(JurassicPark, TyranosaurusRex) as Contains>::Out, True);
        assert_type_eq!(<(JurassicPark, Pterodactyl) as Contains>::Out, False);
    }

    #[test]
    fn insert() {
        type JurassicPark1 = set![Velociraptor, TyranosaurusRex];
        type JurassicPark2 = <(JurassicPark1, Iguanodon) as Insert>::Out;
        assert_type_eq!(<(JurassicPark2, Iguanodon) as Contains>::Out, True);
    }

    #[test]
    fn remove() {
        type MoarDinos = set![Velociraptor, TyranosaurusRex, Pterodactyl, Iguanodon];
        type LessDinos = <(MoarDinos, Pterodactyl) as Remove>::Out;
        assert_type_eq!(<(LessDinos, Pterodactyl) as Contains>::Out, False);
        assert_type_eq!(<(LessDinos, TyranosaurusRex) as Contains>::Out, True);
    }

    #[test]
    fn intersection() {
        type A = set![TyranosaurusRex, Velociraptor];
        type B = set![TyranosaurusRex, Oviraptor];
        type Ex1 = set![TyranosaurusRex];
        assert_type_eq!(<(A, B) as Intersection>::Out, Ex1);

        type C = <(A, Iguanodon) as Insert>::Out;
        type D = <(B, Iguanodon) as Insert>::Out;
        type Ex2 = set![Iguanodon, TyranosaurusRex];
        type CDIntersection = <(C, D) as Intersection>::Out;
        assert_type_eq!(<(CDIntersection, Ex2) as IsEqual>::Out, True);
    }

    #[test]
    fn union() {
        type A = set![TyranosaurusRex];
        type B = set![Compsognathus];
        type AB = <(A, B) as Union>::Out;
        type Ex1 = set![TyranosaurusRex, Compsognathus];
        assert_type_eq!(<(AB, Ex1) as IsEqual>::Out, True);

        type C = <(A, Iguanodon) as Insert>::Out;
        type D = <(B, Iguanodon) as Insert>::Out;
        type CD = <(C, D) as Union>::Out;
        type Ex2 = set![TyranosaurusRex, Compsognathus, Iguanodon];
        assert_type_eq!(<(CD, Ex2) as IsEqual>::Out, True);
    }

    #[test]
    fn difference() {
        type A = set![TyranosaurusRex, Velociraptor];
        type B = set![Compsognathus, TyranosaurusRex];
        type Ex1 = set![Velociraptor];
        type Ex2 = set![Compsognathus];
        assert_type_eq!(<(A, B) as Difference>::Out, Ex1);
        assert_type_eq!(<(B, A) as Difference>::Out, Ex2);
    }

    #[test]
    fn size() {
        use crate::num::consts::{U2, U3, U4};
        type Dinosaurs = set![Oviraptor, TyranosaurusRex, Velociraptor];
        assert_type_eq!(<Dinosaurs as Size>::Out, U3);

        type Bigger = <(Dinosaurs, Compsognathus) as Insert>::Out;
        assert_type_eq!(<Bigger as Size>::Out, U4);

        type Smaller = <(Dinosaurs, Oviraptor) as Remove>::Out;
        assert_type_eq!(<Smaller as Size>::Out, U2);
    }

    #[test]
    fn map() {
        pub struct Wrapper<T>(PhantomData<T>);
        impl<T, U> IsEqual for (Wrapper<T>, Wrapper<U>)
        where
            (T, U): IsEqual,
        {
            type Out = <(T, U) as IsEqual>::Out;
        }

        pub struct Wrap;
        impl<T> Mapper<T> for Wrap {
            type Out = Wrapper<T>;
        }
        type Dinosaurs = set![Oviraptor, TyranosaurusRex, Velociraptor];
        type MappedDinos = <(Dinosaurs, Wrap) as Map<<Dinosaurs as Container>::Content, Wrap>>::Out;
        assert_type_eq!(
            MappedDinos,
            set![
                Wrapper<Oviraptor>,
                Wrapper<TyranosaurusRex>,
                Wrapper<Velociraptor>
            ]
        );
    }
}
