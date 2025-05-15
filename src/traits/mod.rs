pub mod applicative;
pub mod fold;
pub mod functor;
pub mod monad;
pub mod monoid;
pub mod semigroup;

pub type Pure<T> = <T as applicative::Pure<T>>::Out;
