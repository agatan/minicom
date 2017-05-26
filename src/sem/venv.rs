use std::collections::HashMap;

use sem::ir::Type;

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

    pub fn insert(&mut self, name: String, typ: Type) {
        let index = self.types.len();
        self.types.push(typ);
        self.vars.insert(name, index);
    }

    pub fn get(&self, name: &str) -> Option<Type> {
        self.vars.get(name).map(|&index| self.types[index].clone())
    }
}
