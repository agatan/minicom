use std::collections::HashMap;

use sem::ir::{LocalId, FunctionId, Level, Type, Var};

#[derive(Debug)]
pub struct VariableEnv<'a> {
    parent: Option<&'a VariableEnv<'a>>,
    vars: HashMap<String, Entry>,
    n_args: u32,
    n_locals: u32,
    n_functions: u32,
}

impl<'a> VariableEnv<'a> {
    pub fn new() -> Self {
        VariableEnv {
            parent: None,
            vars: HashMap::new(),
            n_args: 0,
            n_locals: 0,
            n_functions: 0,
        }
    }

    pub fn scoped<'parent>(&'parent self) -> VariableEnv<'parent> {
        VariableEnv {
            parent: Some(self),
            vars: HashMap::new(),
            n_args: 0,
            n_locals: 0,
            n_functions: 0,
        }
    }

    pub fn insert_arg(&mut self, name: String, typ: Type) -> u32 {
        let n = self.n_args;
        self.n_args += 1;
        self.vars.insert(name, Entry::Var(Var::Arg(n, typ)));
        n
    }

    pub fn insert_local(&mut self, name: String, typ: Type) -> LocalId {
        let id = LocalId::new(self.n_locals);
        self.n_locals += 1;
        self.vars.insert(name, Entry::Var(Var::Local(id, typ)));
        id
    }

    pub fn insert_function(&mut self, name: String, args: Vec<Type>, ret: Type) -> FunctionId {
        let id = FunctionId::new(self.n_functions);
        self.n_functions += 1;
        self.vars.insert(name, Entry::Function(id, args, ret));
        id
    }

    pub fn get_var(&self, name: &str) -> Option<(Var, Level)> {
        match self.vars.get(name) {
            Some(&Entry::Var(ref v)) => Some((v.clone(), 0)),
            Some(&Entry::Function(_, _, _)) => None,
            None => {
                self.parent
                    .and_then(|env| env.get_var(name).map(|(var, level)| (var, level + 1)))
            }
        }
    }

    pub fn get_function(&self, name: &str) -> Option<(FunctionId, Vec<Type>, Type)> {
        match self.vars.get(name) {
            Some(&Entry::Function(id, ref args, ref ret)) => Some((id, args.clone(), ret.clone())),
            Some(&Entry::Var(_)) => None,
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
