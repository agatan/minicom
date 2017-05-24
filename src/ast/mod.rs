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

    pub fn to_u32(self) -> u32 {
        self.0
    }
}

pub trait Node {
    fn get_id(&self) -> NodeId;
}
