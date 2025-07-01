use typenum::{Bit, UInt, UTerm, Unsigned};

use crate::traits::functor::Mapper;

use super::list::List as L;

pub struct Left;
impl<T, U> Mapper<(T, U)> for Left {
    type Out = T;
}
pub struct Right;
impl<T, U> Mapper<(T, U)> for Right {
    type Out = U;
}

pub trait Value: Tuplify {
    type Out;
    fn value() -> <Self as Value>::Out;
}
pub trait Tuplify {
    type Out;
}
pub trait Detuplify {
    type Out;
}
pub trait Merge {
    type Out;
    fn merge(self) -> <Self as Merge>::Out;
}

impl Tuplify for UTerm {
    type Out = UTerm;
}
impl Value for UTerm {
    type Out = usize;
    fn value() -> usize {
        0
    }
}
impl<U, B> Tuplify for typenum::UInt<U, B> {
    type Out = UInt<U, B>;
}
impl<U, B> Value for UInt<U, B>
where
    U: Unsigned,
    B: Bit,
{
    type Out = usize;
    fn value() -> <Self as Value>::Out {
        <UInt<U, B> as typenum::Unsigned>::USIZE
    }
}

impl<T1> Tuplify for L<(T1, L<()>)>
where
    T1: Tuplify,
{
    type Out = <T1 as Tuplify>::Out;
}
macro_rules! tuplify {
    [$($ts:ident),+] => {
        impl<$($ts: $crate::collections::tuple::Tuplify),+> $crate::collections::tuple::Tuplify for $crate::list![$($ts),+] {
            type Out = ($(<$ts as $crate::collections::tuple::Tuplify>::Out),+);
        }
        impl<$($ts: $crate::collections::tuple::Value),+> $crate::collections::tuple::Value for $crate::list![$($ts),+] {
            type Out = ($(<$ts as $crate::collections::tuple::Value>::Out),+);
            fn value() -> <Self as Value>::Out {
                ($(<$ts as $crate::collections::tuple::Value>::value()),+)
            }
        }
    };
}
tuplify![T1, T2];
tuplify![T1, T2, T3];
tuplify![T1, T2, T3, T4];
tuplify![T1, T2, T3, T4, T5];
tuplify![T1, T2, T3, T4, T5, T6];
tuplify![T1, T2, T3, T4, T5, T6, T7];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13];
tuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14];
tuplify![
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15
];
tuplify![
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16
];

macro_rules! detuplify {
    [$($ts:ident),+] => {
        impl<$($ts),+> $crate::collections::tuple::Detuplify for ($($ts),+) {
            type Out = $crate::list![$($ts),+];
        }
    };
}
detuplify![T1, T2];
detuplify![T1, T2, T3];
detuplify![T1, T2, T3, T4];
detuplify![T1, T2, T3, T4, T5];
detuplify![T1, T2, T3, T4, T5, T6];
detuplify![T1, T2, T3, T4, T5, T6, T7];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13];
detuplify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14];
detuplify![
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15
];
detuplify![
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16
];

#[cfg(test)]
mod test {
    use super::*;
    use crate::list;
    use core::marker::PhantomData;
    use typenum::assert_type_eq;

    #[test]
    fn tuplify() {
        struct T<U>(PhantomData<U>);
        impl<U> Tuplify for T<U> {
            type Out = U;
        }
        assert_type_eq!(
            <list![T<u32>, T<usize>, T<bool>] as Tuplify>::Out,
            (u32, usize, bool)
        );
        assert_type_eq!(
            <list![
                list![T<u32>, T<bool>],
                T<usize>,
                list![T<bool>, T<bool>, list![T<usize>, T<u32>]]
            ] as Tuplify>::Out,
            ((u32, bool), usize, (bool, bool, (usize, u32)))
        );
    }

    #[test]
    fn detuplify() {
        assert_type_eq!(
            <(bool, u32, usize) as Detuplify>::Out,
            list![bool, u32, usize]
        );
    }

    #[test]
    fn numbers() {
        use typenum::{U1, U2, U3};
        assert_eq!(<list![U1, U2, U3] as Value>::value(), (1, 2, 3))
    }

    #[test]
    fn dinos() {
        use crate::dinosaurs::*;

        #[derive(Debug, PartialEq, Eq)]
        pub struct Laramidia;
        #[derive(Debug, PartialEq, Eq)]
        pub struct Asia;

        pub struct Origin<T>(PhantomData<T>);
        impl Tuplify for Origin<TyranosaurusRex> {
            type Out = Laramidia;
        }
        impl Tuplify for Origin<TarbosaurusBataar> {
            type Out = Asia;
        }

        assert_type_eq!(
            <list![Origin<TyranosaurusRex>, Origin<TarbosaurusBataar>] as Tuplify>::Out,
            (Laramidia, Asia)
        );

        impl Value for Origin<TyranosaurusRex> {
            type Out = Laramidia;
            fn value() -> <Self as Value>::Out {
                Laramidia
            }
        }
        impl Value for Origin<TarbosaurusBataar> {
            type Out = Asia;
            fn value() -> <Self as Value>::Out {
                Asia
            }
        }
        assert_eq!(
            <list![Origin<TarbosaurusBataar>, Origin<TyranosaurusRex>] as Value>::value(),
            (Asia, Laramidia)
        );
    }
}
