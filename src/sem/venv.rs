use std::collections::HashMap;

use sem::ir::{IdentId, Level, Type};

#[derive(Debug)]
pub struct VariableEnv<'a> {
    parent: Option<&'a VariableEnv<'a>>,
    entries: Vec<Entry>,
    vars: HashMap<String, usize>,
}

impl<'a> VariableEnv<'a> {
    pub fn new() -> Self {
        VariableEnv {
            parent: None,
            entries: Vec::new(),
            vars: HashMap::new(),
        }
    }

    pub fn scoped<'parent>(&'parent self) -> VariableEnv<'parent> {
        VariableEnv {
            parent: Some(self),
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

    pub fn insert_function(&mut self, name: String, args: Vec<Type>, ret: Type) -> IdentId {
        let index = self.entries.len();
        self.entries.push(Entry::Function(args, ret));
        self.vars.insert(name, index);
        IdentId::new(index as u32)
    }

    pub fn get_var(&self, name: &str) -> Option<(IdentId, Level, Type)> {
        self.vars
            .get(name)
            .and_then(|&index| {
                match self.entries[index] {
                    Entry::Var(ref typ) => Some((IdentId::new(index as u32), 0, typ.clone())),
                    Entry::Function(_, _) => None,
                }
            })
    }
}

#[derive(Debug)]
pub enum Entry {
    Var(Type),
    Function(Vec<Type>, Type),
}
