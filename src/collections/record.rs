use core::marker::PhantomData;

use crate::{
    bool::{monoid::Either, False, Not, True},
    cmp::{EqualTo, IsEqualTo},
    traits::{
        fold::Foldable,
        functor::{Map, Mapper},
        semigroup::Mappend,
    },
};

use super::{
    list::{self, Any, Filter, Interleave, List, UntupleLeft, UntupleRight, ZipTuple},
    maybe::{Just, Nothing},
    set::{self, Set},
    tuple, Container,
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

pub trait MapKeys {
    type Out;
}
impl<T, U, M> MapKeys for (Record<Set<T>, U>, M)
where
    T: Container,
    U: Container,
    (Set<T>, M): Map<<T as Container>::Content, M>,
    (U, tuple::Left): Map<<U as Container>::Content, tuple::Left>,
    (U, tuple::Right): Map<<U as Container>::Content, tuple::Right>,
    UntupleRight<U>: Container,
    UntupleLeft<U>: Container,
    (UntupleLeft<U>, M): Map<<UntupleLeft<U> as Container>::Content, M>,
    (
        <(UntupleLeft<U>, M) as Map<<UntupleLeft<U> as Container>::Content, M>>::Out,
        UntupleRight<U>,
    ): ZipTuple,
{
    type Out = Record<
        <(Set<T>, M) as Map<<T as Container>::Content, M>>::Out,
        <(
            <(UntupleLeft<U>, M) as Map<<UntupleLeft<U> as Container>::Content, M>>::Out,
            UntupleRight<U>,
        ) as ZipTuple>::Out,
    >;
}

pub trait Fields {
    type Out;
}
impl<K, F> Fields for Record<Set<K>, F> {
    type Out = F;
}

pub struct WithInterleaveFrom<R>(PhantomData<R>);
impl<K, V, R> Mapper<(K, V)> for WithInterleaveFrom<R>
where
    (R, K): ContainsKey,
    (R, K): GetEntry,
    (V, <(R, K) as GetEntry>::Out): Interleave,
{
    type Out = (K, <(V, <(R, K) as GetEntry>::Out) as Interleave>::Out);
}

pub struct WithMappendFrom<R>(PhantomData<R>);
impl<K, V, R> Mapper<(K, V)> for WithMappendFrom<R>
where
    (R, K): ContainsKey,
    (R, K): GetEntry,
    (V, <(R, K) as GetEntry>::Out): Mappend,
{
    type Out = (K, <(V, <(R, K) as GetEntry>::Out) as Mappend>::Out);
}

pub trait Merge {
    type Out;
}
impl<K1, F1, K2, F2> Merge for (Record<Set<K1>, F1>, Record<Set<K2>, F2>)
where
    (Set<K1>, Set<K2>): set::Difference,
    (Set<K2>, Set<K1>): set::Difference,
    (Set<K1>, Set<K2>): set::Intersection,
    (Set<K1>, Set<K2>): set::Union,
    (
        Record<Set<K1>, F1>,
        <(Set<K1>, Set<K2>) as set::Intersection>::Out,
    ): RemoveKeys,
    (
        Record<Set<K2>, F2>,
        <(Set<K1>, Set<K2>) as set::Intersection>::Out,
    ): RemoveKeys,
    (
        Record<Set<K1>, F1>,
        <(Set<K1>, Set<K2>) as set::Difference>::Out,
    ): RemoveKeys,
    (
        Record<Set<K2>, F2>,
        <(Set<K2>, Set<K1>) as set::Difference>::Out,
    ): RemoveKeys,
    <(
        Record<Set<K1>, F1>,
        <(Set<K1>, Set<K2>) as set::Difference>::Out,
    ) as RemoveKeys>::Out: Fields,
    <(
        Record<Set<K1>, F1>,
        <(Set<K1>, Set<K2>) as set::Intersection>::Out,
    ) as RemoveKeys>::Out: Fields,
    <(
        Record<Set<K2>, F2>,
        <(Set<K1>, Set<K2>) as set::Intersection>::Out,
    ) as RemoveKeys>::Out: Fields,
    <<(
        Record<Set<K1>, F1>,
        <(Set<K1>, Set<K2>) as set::Difference>::Out,
    ) as RemoveKeys>::Out as Fields>::Out: Container,
    (
        <<(
            Record<Set<K1>, F1>,
            <(Set<K1>, Set<K2>) as set::Difference>::Out,
        ) as RemoveKeys>::Out as Fields>::Out,
        WithInterleaveFrom<
            <(
                Record<Set<K2>, F2>,
                <(Set<K2>, Set<K1>) as set::Difference>::Out,
            ) as RemoveKeys>::Out,
        >,
    ): Map<
        <<<(
            Record<Set<K1>, F1>,
            <(Set<K1>, Set<K2>) as set::Difference>::Out,
        ) as RemoveKeys>::Out as Fields>::Out as Container>::Content,
        WithInterleaveFrom<
            <(
                Record<Set<K2>, F2>,
                <(Set<K2>, Set<K1>) as set::Difference>::Out,
            ) as RemoveKeys>::Out,
        >,
    >,
    (
        <<(
            Record<Set<K1>, F1>,
            <(Set<K1>, Set<K2>) as set::Intersection>::Out,
        ) as RemoveKeys>::Out as Fields>::Out,
        <<(
            Record<Set<K2>, F2>,
            <(Set<K1>, Set<K2>) as set::Intersection>::Out,
        ) as RemoveKeys>::Out as Fields>::Out,
    ): Merge,
    (
        <(
            <<(
                Record<Set<K1>, F1>,
                <(Set<K1>, Set<K2>) as set::Intersection>::Out,
            ) as RemoveKeys>::Out as Fields>::Out,
            <<(
                Record<Set<K2>, F2>,
                <(Set<K1>, Set<K2>) as set::Intersection>::Out,
            ) as RemoveKeys>::Out as Fields>::Out,
        ) as Merge>::Out,
        <(
            <<(
                Record<Set<K1>, F1>,
                <(Set<K1>, Set<K2>) as set::Difference>::Out,
            ) as RemoveKeys>::Out as Fields>::Out,
            WithInterleaveFrom<
                <(
                    Record<Set<K2>, F2>,
                    <(Set<K2>, Set<K1>) as set::Difference>::Out,
                ) as RemoveKeys>::Out,
            >,
        ) as Map<
            <<<(
                Record<Set<K1>, F1>,
                <(Set<K1>, Set<K2>) as set::Difference>::Out,
            ) as RemoveKeys>::Out as Fields>::Out as Container>::Content,
            WithInterleaveFrom<
                <(
                    Record<Set<K2>, F2>,
                    <(Set<K2>, Set<K1>) as set::Difference>::Out,
                ) as RemoveKeys>::Out,
            >,
        >>::Out,
    ): Merge,
{
    type Out = Record<
        <(Set<K1>, Set<K2>) as set::Union>::Out,
        <(
            <(
                <<(
                    Record<Set<K1>, F1>,
                    <(Set<K1>, Set<K2>) as set::Intersection>::Out,
                ) as RemoveKeys>::Out as Fields>::Out,
                <<(
                    Record<Set<K2>, F2>,
                    <(Set<K1>, Set<K2>) as set::Intersection>::Out,
                ) as RemoveKeys>::Out as Fields>::Out,
            ) as Merge>::Out,
            <(
                <<(
                    Record<Set<K1>, F1>,
                    <(Set<K1>, Set<K2>) as set::Difference>::Out,
                ) as RemoveKeys>::Out as Fields>::Out,
                WithInterleaveFrom<
                    <(
                        Record<Set<K2>, F2>,
                        <(Set<K2>, Set<K1>) as set::Difference>::Out,
                    ) as RemoveKeys>::Out,
                >,
            ) as Map<
                <<<(
                    Record<Set<K1>, F1>,
                    <(Set<K1>, Set<K2>) as set::Difference>::Out,
                ) as RemoveKeys>::Out as Fields>::Out as Container>::Content,
                WithInterleaveFrom<
                    <(
                        Record<Set<K2>, F2>,
                        <(Set<K2>, Set<K1>) as set::Difference>::Out,
                    ) as RemoveKeys>::Out,
                >,
            >>::Out,
        ) as Merge>::Out,
    >;
}

pub trait InsertField {
    type Out;
}
impl<Keys, Fields, K, V> InsertField for (Record<Set<Keys>, Fields>, (K, V))
where
    (Set<Keys>, K): set::Insert,
    (Fields, List<((K, V), list::Empty)>): Interleave,
{
    type Out = Record<
        <(Set<Keys>, K) as set::Insert>::Out,
        <(Fields, List<((K, V), list::Empty)>) as Interleave>::Out,
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

pub trait GetEntryContained {
    type Out;
}
impl<Keys, Fields, K> GetEntryContained for (Record<Set<Keys>, Fields>, K, False) {
    type Out = Nothing;
}
impl<Keys, Fields, K> GetEntryContained for (Record<Set<Keys>, Fields>, K, True)
where
    (Record<Set<Keys>, Fields>, K): GetEntry,
{
    type Out = Just<<(Record<Set<Keys>, Fields>, K) as GetEntry>::Out>;
}

pub trait GetEntryMaybe {
    type Out;
}
impl<Keys, Fields, K> GetEntryMaybe for (Record<Set<Keys>, Fields>, K)
where
    (Record<Set<Keys>, Fields>, K): ContainsKey,
    (
        Record<Set<Keys>, Fields>,
        K,
        <(Record<Set<Keys>, Fields>, K) as ContainsKey>::Out,
    ): GetEntryContained,
{
    type Out = <(
        Record<Set<Keys>, Fields>,
        K,
        <(Record<Set<Keys>, Fields>, K) as ContainsKey>::Out,
    ) as GetEntryContained>::Out;
}
pub type GetMaybe<R, K> = <(R, K) as GetEntryMaybe>::Out;

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

pub trait Values {
    type Out;
}
impl<K, V> Values for Record<K, V>
where
    V: Container,
    (V, tuple::Right): Map<<V as Container>::Content, tuple::Right>,
{
    type Out = UntupleRight<V>;
}

#[macro_export]
macro_rules! record {
    {$(,)?} => {
        $crate::collections::record::Record<$crate::set![], $crate::collections::list::Empty>
    };
    {$($ks:ty:$vs:ty),*$(,)?} => {
        $crate::collections::record::Record<$crate::set![$($ks),*], $crate::list![$(($ks, $vs)),*]>
    };
}

#[macro_export]
macro_rules! merge_records {
    [$r:ty$(,)?] => {
        $r
    };
    [$r:ty,$($rs:ty),*$(,)?] => {
        <($r, $crate::merge_records![$($rs),*]) as $crate::collections::record::Merge>::Out
    }
}

#[allow(unused)]
#[cfg(test)]
mod test {
    use crate::bool::{False, True};
    use crate::dinosaurs::*;
    use crate::{assert_type_eq, elements, list, record, set};

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

        assert_type_eq!(GetMaybe<DinosaurSizes, Pterodactyl>, Nothing);
        assert_type_eq!(GetMaybe<DinosaurSizes, TyranosaurusRex>, Just<Large>);
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

    //#[test]
    //fn map() {
    //    use crate::cmp::IsEqual;
    //
    //    pub struct Wrapper<T>(PhantomData<T>);
    //    impl<T, U> IsEqual for (Wrapper<T>, Wrapper<U>)
    //    where
    //        (T, U): IsEqual,
    //    {
    //        type Out = <(T, U) as IsEqual>::Out;
    //    }
    //
    //    pub struct Wrap;
    //    impl<T> Mapper<T> for Wrap {
    //        type Out = Wrapper<T>;
    //    }
    //
    //    type DinosaurSizes = record! {
    //        Oviraptor: Small,
    //        Velociraptor: Medium,
    //        TyranosaurusRex: Large
    //    };
    //    type MappedDinos = <(DinosaurSizes, Wrap) as MapKeys>::Out;
    //
    //    assert_type_eq!(
    //        record! {
    //            Wrapper<Oviraptor>: Small,
    //            Wrapper<Velociraptor>: Medium,
    //            Wrapper<TyranosaurusRex>: Large
    //        },
    //        MappedDinos
    //    );
    //}

    #[test]
    fn merge() {
        use crate::list;
        use crate::num::consts::*;

        type DinosaurNumsA = record! {
            Oviraptor: list![U1, U2, U3],
            Velociraptor: list![U1, U2, U3],
            TyranosaurusRex: list![U1, U2, U3]
        };
        type DinosaurNumsB = record! {
            Oviraptor: list![U4, U5, U6],
            Velociraptor: list![U4, U5, U6],
            Compsognathus: list![U1]
        };
        type Merged = <(DinosaurNumsA, DinosaurNumsB) as Merge>::Out;

        assert_type_eq!(<Merged as Size>::Out, U4);
        assert_type_eq!(
            Get<Merged, Oviraptor>,
            list![U1, U2, U3, U4, U5, U6]
        );
        assert_type_eq!(
            Get<Merged, Velociraptor>,
            list![U1, U2, U3, U4, U5, U6]
        );
        assert_type_eq!(
            Get<Merged, TyranosaurusRex>,
            list![U1, U2, U3]
        );
        assert_type_eq!(
            Get<Merged, Compsognathus>,
            list![U1]
        );
    }

    #[test]
    fn values() {
        type DinosaurSizes = record! {
            Oviraptor: Small,
            Velociraptor: Medium,
            TyranosaurusRex: Large
        };
        assert_type_eq!(<DinosaurSizes as Values>::Out, list![Small, Medium, Large]);
    }
}
