use core::marker::PhantomData;

use crate::{
    bool::{Not, monoid::Either},
    cmp::{EqualTo, IsEqualTo},
    traits::{
        fold::Foldable,
        functor::{Map, Mapper},
        semigroup::Mappend,
    },
};

use super::{
    Container,
    list::{self, Any, Filter, List},
    set::{self, Set},
    tuple,
};

pub struct Record<Keys, Fields>(PhantomData<Keys>, PhantomData<Fields>);
pub type Empty = Record<set::Empty, list::Empty>;

pub trait ContainsKey {
    type Out;
}
impl<T, U, K> ContainsKey for (Record<Set<T>, U>, K)
where
    T: Container,
    (T, IsEqualTo<K>): Map<<T as Container>::Content, IsEqualTo<K>>,
    <(T, IsEqualTo<K>) as Map<<T as Container>::Content, IsEqualTo<K>>>::Out: Foldable<Either>,
{
    type Out = Any<EqualTo<T, K>>;
}

pub trait InsertField {
    type Out;
}
impl<Keys, Fields, K, V> InsertField for (Record<Set<Keys>, Fields>, (K, V))
where
    (Set<Keys>, K): set::Insert,
    (Fields, List<((K, V), list::Empty)>): Mappend,
{
    type Out = Record<
        <(Set<Keys>, K) as set::Insert>::Out,
        <(Fields, List<((K, V), list::Empty)>) as Mappend>::Out,
    >;
}
pub type Insert<Record, Field> = <(Record, Field) as InsertField>::Out;

pub struct WithLeftEqualTo<U>(PhantomData<U>);
impl<T, U, V> Mapper<(T, U)> for WithLeftEqualTo<V>
where
    (T, V): crate::cmp::IsEqual,
{
    type Out = ((T, U), <(T, V) as crate::cmp::IsEqual>::Out);
}
pub type WithLeftEquals<T, U> =
    <(T, WithLeftEqualTo<U>) as Map<<T as Container>::Content, WithLeftEqualTo<U>>>::Out;

pub struct WithLeftNotEqualTo<U>(PhantomData<U>);
impl<T, U, V> Mapper<(T, U)> for WithLeftNotEqualTo<V>
where
    (T, V): crate::cmp::IsNotEqual,
{
    type Out = ((T, U), <(T, V) as crate::cmp::IsNotEqual>::Out);
}
pub type WithLeftNotEquals<T, U> =
    <(T, WithLeftNotEqualTo<U>) as Map<<T as Container>::Content, WithLeftNotEqualTo<U>>>::Out;

pub trait GetEntry {
    type Out;
}
impl<Keys, Fields, K> GetEntry for (Record<Set<Keys>, Fields>, K)
where
    (Record<Set<Keys>, Fields>, K): ContainsKey,
    Fields: Container,
    (Fields, tuple::Left): Map<<Fields as Container>::Content, tuple::Left>,
    (Fields, WithLeftEqualTo<K>): Map<<Fields as Container>::Content, WithLeftEqualTo<K>>,
    WithLeftEquals<Fields, K>: Foldable<Filter>,
    <WithLeftEquals<Fields, K> as Foldable<Filter>>::Out: Container,
    (
        <WithLeftEquals<Fields, K> as Foldable<Filter>>::Out,
        tuple::Right,
    ): Map<
            <<WithLeftEquals<Fields, K> as Foldable<Filter>>::Out as Container>::Content,
            tuple::Right,
        >,
    <(
        <WithLeftEquals<Fields, K> as Foldable<Filter>>::Out,
        tuple::Right,
    ) as Map<
        <<WithLeftEquals<Fields, K> as Foldable<Filter>>::Out as Container>::Content,
        tuple::Right,
    >>::Out: Container,
{
    type Out = <<(
        <WithLeftEquals<Fields, K> as Foldable<Filter>>::Out,
        tuple::Right,
    ) as Map<
        <<WithLeftEquals<Fields, K> as Foldable<Filter>>::Out as Container>::Content,
        tuple::Right,
    >>::Out as Container>::Content;
}
pub type Get<Record, Key> = <(Record, Key) as GetEntry>::Out;

pub trait RemoveEntry {
    type Out;
}
impl<Keys, Fields, K> RemoveEntry for (Record<Set<Keys>, Fields>, K)
where
    (Record<Set<Keys>, Fields>, K): ContainsKey,
    (Set<Keys>, K): set::Remove,
    Fields: Container,
    (Fields, tuple::Left): Map<<Fields as Container>::Content, tuple::Left>,
    (Fields, WithLeftNotEqualTo<K>): Map<<Fields as Container>::Content, WithLeftNotEqualTo<K>>,
    WithLeftNotEquals<Fields, K>: Foldable<Filter>,
{
    type Out = Record<
        <(Set<Keys>, K) as set::Remove>::Out,
        <WithLeftNotEquals<Fields, K> as Foldable<Filter>>::Out,
    >;
}
pub type Remove<Record, Key> = <(Record, Key) as RemoveEntry>::Out;

pub struct WithLeftNotIn<U>(PhantomData<U>);
impl<T, U, V> Mapper<(T, U)> for WithLeftNotIn<V>
where
    (V, T): set::Contains,
    <(V, T) as set::Contains>::Out: Not,
{
    type Out = ((T, U), <<(V, T) as set::Contains>::Out as Not>::Out);
}
pub type WithoutContainedBy<T, U> =
    <(T, WithLeftNotIn<U>) as Map<<T as Container>::Content, WithLeftNotIn<U>>>::Out;

pub trait RemoveKeys {
    type Out;
}
impl<Keys, Fields, Ks> RemoveKeys for (Record<Set<Keys>, Fields>, Ks)
where
    (Set<Keys>, Ks): set::Difference,
    Fields: Container,
    (Fields, WithLeftNotIn<Ks>): Map<<Fields as Container>::Content, WithLeftNotIn<Ks>>,
    WithoutContainedBy<Fields, Ks>: Foldable<Filter>,
{
    type Out = Record<
        <(Set<Keys>, Ks) as set::Difference>::Out,
        <WithoutContainedBy<Fields, Ks> as Foldable<Filter>>::Out,
    >;
}

pub trait Size {
    type Out;
}
impl<Keys, Fields> Size for Record<Set<Keys>, Fields>
where
    Set<Keys>: set::Size,
{
    type Out = <Set<Keys> as set::Size>::Out;
}

#[macro_export]
macro_rules! record {
    {} => {
        $crate::collections::record::Record<$crate::set![], $crate::collections::list::Empty>
    };
    {$($ks:ty:$vs:ty),*} => {
        $crate::collections::record::Record<$crate::set![$($ks),*], $crate::list![$(($ks, $vs)),*]>
    };
}

#[allow(unused)]
#[cfg(test)]
mod test {
    use crate::bool::{False, True};
    use crate::dinosaurs::*;
    use crate::{assert_type_eq, elements, record, set};

    use super::*;

    struct Small;
    struct Medium;
    struct Large;

    #[test]
    fn record_macro() {
        type DinosaurSizes = record! {
            Oviraptor: Small,
            Velociraptor: Medium,
            TyranosaurusRex: Large
        };
        assert_type_eq!(Get<DinosaurSizes, Oviraptor>, Small);
        assert_type_eq!(Get<DinosaurSizes, Velociraptor>, Medium);
        assert_type_eq!(Get<DinosaurSizes, TyranosaurusRex>, Large);
    }

    #[test]
    fn insert() {
        type Sizes = record! {
            TyranosaurusRex: Large
        };

        type WithCompsognathus = Insert<Sizes, (Compsognathus, Small)>;
        assert_type_eq!(Get<WithCompsognathus, Compsognathus>, Small);
    }

    #[test]
    fn remove() {
        type Sizes = record! {
            TyranosaurusRex: Large,
            Compsognathus: Small
        };
        assert_type_eq!(<(Sizes, Compsognathus) as ContainsKey>::Out, True);

        type WithoutCompsognathus = Remove<Sizes, Compsognathus>;
        assert_type_eq!(
            <(WithoutCompsognathus, Compsognathus) as ContainsKey>::Out,
            False
        );
    }

    #[test]
    fn size() {
        use crate::num::consts::{U2, U3, U4};
        type DinosaurSizes = record! {
            Oviraptor: Small,
            Velociraptor: Medium,
            TyranosaurusRex: Large
        };
        assert_type_eq!(<DinosaurSizes as Size>::Out, U3);

        type Bigger = Insert<DinosaurSizes, (Compsognathus, Small)>;
        assert_type_eq!(<Bigger as Size>::Out, U4);

        type Smaller = Remove<DinosaurSizes, Oviraptor>;
        assert_type_eq!(<Smaller as Size>::Out, U2);
    }

    #[test]
    fn remove_keys() {
        type DinosaurSizes = record! {
            Oviraptor: Small,
            Velociraptor: Medium,
            TyranosaurusRex: Large
        };
        type Keys = set![Oviraptor, Velociraptor];
        type Removed = <(DinosaurSizes, Keys) as RemoveKeys>::Out;
        assert_type_eq!(Removed, record! { TyranosaurusRex: Large });

        type TRex = set![TyranosaurusRex];
        type Cleared = <(Removed, TRex) as RemoveKeys>::Out;
        assert_type_eq!(Cleared, Empty);
    }
}
