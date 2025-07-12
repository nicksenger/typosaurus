pub trait Semigroup<Lhs, Rhs> {
    type Mappend;
}

pub trait Mappend {
    type Out;
}

pub trait MappendG<T> {
    type Out;
}
impl<T, U> Mappend for (T, U)
where
    T: MappendG<U>,
{
    type Out = <T as MappendG<U>>::Out;
}
