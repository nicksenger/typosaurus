pub trait Semigroup<Lhs, Rhs> {
    type Mappend;
}

pub trait Mappend {
    type Out;
}
