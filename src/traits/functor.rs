pub trait Functor<T, M> {}
pub trait Map<T, M> {
    type Out;
}
pub trait Mapper<T> {
    type Out;
}
