mod expr;
mod types;
pub use self::expr::*;
pub use self::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }
}

pub trait Node {
    fn get_id(&self) -> NodeId;
}
