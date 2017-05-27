use std::collections::HashMap;

use sem::ir::{IdentId, Type, Function};

#[derive(Debug)]
pub struct VariableEnv {
    entries: Vec<Entry>,
    vars: HashMap<String, usize>,
}

impl VariableEnv {
    pub fn new() -> Self {
        VariableEnv {
            entries: Vec::new(),
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, typ: Type) -> IdentId {
        let index = self.entries.len();
        self.entries.push(Entry::Var(typ));
        self.vars.insert(name, index);
        IdentId::new(index as u32)
    }

    pub fn get_var(&self, name: &str) -> Option<(IdentId, Type)> {
        self.vars
            .get(name)
            .and_then(|&index| {
                match self.entries[index] {
                    Entry::Var(ref typ) => Some((IdentId::new(index as u32), typ.clone())),
                    Entry::Function(_) => None,
                }
            })
    }
}

#[derive(Debug)]
pub enum Entry {
    Var(Type),
    Function(Function),
}
