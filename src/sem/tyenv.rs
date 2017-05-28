use std::collections::HashMap;

use sem::ir::Type;
use sem::{Error, ErrorKind, Result};

#[derive(Debug)]
pub struct TypeEnv {
    table: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut env = TypeEnv { table: HashMap::new() };
        env.insert("int".to_string(), Type::Int);
        env.insert("float".to_string(), Type::Float);
        env.insert("bool".to_string(), Type::Bool);
        env.insert("()".to_string(), Type::Unit);
        env
    }

    fn insert(&mut self, name: String, typ: Type) {
        self.table.insert(name, typ);
    }

    pub fn get(&self, name: &str) -> Result<Type> {
        self.table
            .get(name)
            .ok_or(Error::from(ErrorKind::Undefined(name.to_string())))
            .map(Type::clone)
    }
}
