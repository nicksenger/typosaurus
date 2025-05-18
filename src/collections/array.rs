use typenum::{Bit, UInt, UTerm, Unsigned};

use super::list::List as L;

pub trait Arrayify<T> {
    type Out;
    fn value() -> Self::Out;
}

impl Arrayify<usize> for UTerm {
    type Out = usize;
    fn value() -> usize {
        0
    }
}
impl<U, B> Arrayify<usize> for UInt<U, B>
where
    U: Unsigned,
    B: Bit,
{
    type Out = usize;
    fn value() -> usize {
        <UInt<U, B> as typenum::Unsigned>::USIZE
    }
}

macro_rules! arrayify {
    [$t:ident,$($ts:ident),*;$n:expr] => {
        impl<T, $t: $crate::collections::array::Arrayify<T, Out = T>,$($ts: $crate::collections::array::Arrayify<T, Out = T>),*> $crate::collections::array::Arrayify<T> for $crate::list![$t,$($ts),*] {
            type Out = [T; $n];
            fn value() -> [T; $n] {
                [<$t as $crate::collections::array::Arrayify<T>>::value(),$(<$ts as $crate::collections::array::Arrayify<T>>::value()),*]
            }
        }

        impl<T, $t: $crate::collections::array::Arrayify<T, Out = T>,$($ts: $crate::collections::array::Arrayify<T, Out = T>),*> $crate::collections::array::Arrayify<T> for $crate::value_list_ty![$t,$($ts),*] {
            type Out = [T; $n];
            fn value() -> [T; $n] {
                [<$t as $crate::collections::array::Arrayify<T>>::value(),$(<$ts as $crate::collections::array::Arrayify<T>>::value()),*]
            }
        }
    }
}
impl<T, T1: Arrayify<T, Out = T>> Arrayify<T> for L<(T1, L<()>)> {
    type Out = [T; 1];
    fn value() -> [T; 1] {
        [<T1 as Arrayify<T>>::value(); 1]
    }
}
arrayify![T1, T2; 2];
arrayify![T1, T2, T3; 3];
arrayify![T1, T2, T3, T4; 4];
arrayify![T1, T2, T3, T4, T5; 5];
arrayify![T1, T2, T3, T4, T5, T6; 6];
arrayify![T1, T2, T3, T4, T5, T6, T7; 7];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8; 8];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9; 9];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10; 10];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11; 11];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12; 12];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13; 13];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14; 14];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15; 15];
arrayify![T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16; 16];

#[cfg(test)]
mod test {
    use super::*;
    use crate::list;

    #[test]
    fn arrayify() {
        use typenum::{U1, U2, U3};
        assert_eq!(<list![U1, U2, U3] as Arrayify<usize>>::value(), [1, 2, 3]);
    }

    #[test]
    fn dinos() {
        use crate::dinosaurs::*;

        #[derive(Debug, PartialEq, Eq)]
        pub enum Size {
            Small,
            Medium,
            Massive,
        }

        impl Arrayify<Size> for TyranosaurusRex {
            type Out = Size;
            fn value() -> Self::Out {
                Size::Massive
            }
        }
        impl Arrayify<Size> for Argentinosaurus {
            type Out = Size;
            fn value() -> Self::Out {
                Size::Massive
            }
        }
        impl Arrayify<Size> for Compsognathus {
            type Out = Size;
            fn value() -> Self::Out {
                Size::Small
            }
        }
        impl Arrayify<Size> for Triceratops {
            type Out = Size;
            fn value() -> Self::Out {
                Size::Medium
            }
        }

        type Dinos = list![Triceratops, Argentinosaurus, Compsognathus, TyranosaurusRex];
        assert_eq!(
            <Dinos as Arrayify<Size>>::value(),
            [Size::Medium, Size::Massive, Size::Small, Size::Massive]
        );
    }
}
