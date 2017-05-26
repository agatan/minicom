use std::collections::HashMap;

use sem::ir::{IdentId, Type};

#[derive(Debug)]
pub struct VariableEnv {
    types: Vec<Type>,
    vars: HashMap<String, usize>,
}

impl VariableEnv {
    pub fn new() -> Self {
        VariableEnv {
            types: Vec::new(),
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, typ: Type) -> IdentId {
        let index = self.types.len();
        self.types.push(typ);
        self.vars.insert(name, index);
        IdentId::new(index as u32)
    }

    pub fn get(&self, name: &str) -> Option<(IdentId, Type)> {
        self.vars.get(name).map(|&index| (IdentId::new(index as u32), self.types[index].clone()))
    }

    pub fn size(&self) -> u32 {
        self.types.len() as u32
    }
}
