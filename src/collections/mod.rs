pub mod array;
pub mod graph;
pub mod list;
pub mod record;
pub mod set;
pub mod tuple;
pub mod value_list;

pub trait Container {
    type Content;
}
