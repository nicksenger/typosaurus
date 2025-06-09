pub mod array;
pub mod list;
pub mod tuple;
pub mod value_list;

pub trait Container {
    type Content;
}
