pub trait Applicative<T, F> {}
pub trait Pure<T> {
    type Out;
}
pub trait FMap<T, F>
where
    F: Fmapper<T>,
{
    type Out;
}
pub trait Fmapper<T> {
    type Out;
}
