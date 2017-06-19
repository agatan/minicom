use std::collections::HashMap;

use sem::ir::Type;
use sem::Error;

#[derive(Debug)]
pub struct TypeEnv {
    table: HashMap<String, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut env = TypeEnv { table: HashMap::new() };
        env.insert("Int".to_string(), Type::Int);
        env.insert("Float".to_string(), Type::Float);
        env.insert("Bool".to_string(), Type::Bool);
        env.insert("()".to_string(), Type::Unit);
        env
    }

    fn insert(&mut self, name: String, typ: Type) {
        self.table.insert(name, typ);
    }

    pub fn get(&self, name: &str) -> Result<Type, Error> {
        self.table
            .get(name)
            .ok_or(format!("undefined type: {}", name).into())
            .map(Type::clone)
    }
}
