use std::collections::HashMap;

use sem::{ErrorKind, Result};
use sem::ir::{Type, Node, NodeKind};
use ast::{NodeId, Node as AstNode, NodeKind as AstNodeKind};

#[derive(Debug)]
pub struct TypeMap {
    table: HashMap<NodeId, Type>,
}

impl TypeMap {
    pub fn new() -> Self {
        TypeMap { table: HashMap::new() }
    }

    pub fn insert(&mut self, id: NodeId, v: Type) {
        self.table.insert(id, v);
    }

    pub fn get(&self, id: NodeId) -> Type {
        self.table.get(&id).cloned().expect("all ast node should be define in type map")
    }
}
