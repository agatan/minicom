use std::collections::HashMap;

use sem::ir::{LocalId, FunctionId, Level, Type};

#[derive(Debug)]
pub struct VariableEnv<'a> {
    parent: Option<&'a VariableEnv<'a>>,
    vars: HashMap<String, Entry>,
    n_locals: u32,
    n_functions: u32,
}

impl<'a> VariableEnv<'a> {
    pub fn new() -> Self {
        VariableEnv {
            parent: None,
            vars: HashMap::new(),
            n_locals: 0,
            n_functions: 0,
        }
    }

    pub fn scoped<'parent>(&'parent self) -> VariableEnv<'parent> {
        VariableEnv {
            parent: Some(self),
            vars: HashMap::new(),
            n_locals: 0,
            n_functions: 0,
        }
    }

    pub fn insert(&mut self, name: String, typ: Type) -> LocalId {
        let id = LocalId::new(self.n_locals);
        self.n_locals += 1;
        self.vars.insert(name, Entry::Var(id, typ));
        id
    }

    pub fn insert_function(&mut self, name: String, args: Vec<Type>, ret: Type) -> FunctionId {
        let id = FunctionId::new(self.n_functions);
        self.n_functions += 1;
        self.vars.insert(name, Entry::Function(id, args, ret));
        id
    }

    pub fn get_var(&self, name: &str) -> Option<(LocalId, Level, Type)> {
        match self.vars.get(name) {
            Some(&Entry::Var(id, ref typ)) => Some((id, 0, typ.clone())),
            Some(&Entry::Function(_, _, _)) => None,
            None => {
                self.parent
                    .and_then(|env| env.get_var(name).map(|(id, level, typ)| (id, level + 1, typ)))
            }
        }
    }

    pub fn get_function(&self, name: &str) -> Option<(FunctionId, Vec<Type>, Type)> {
        match self.vars.get(name) {
            Some(&Entry::Function(id, ref args, ref ret)) => Some((id, args.clone(), ret.clone())),
            Some(&Entry::Var(_, _)) => None,
            None => {
                self.parent
                    .and_then(|env| {
                        env.get_function(name)
                            .map(|(id, args, ret)| (id, args, ret))
                    })
            }
        }
    }
}

#[derive(Debug)]
pub enum Entry {
    Var(LocalId, Type),
    Function(FunctionId, Vec<Type>, Type),
}
