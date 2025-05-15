pub mod array;
pub mod list;
pub mod tuple;

pub trait Container {
    type Content;
}
