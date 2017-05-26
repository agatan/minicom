use std::collections::HashMap;

use sem::ir::Type;
use ast::NodeId;

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
