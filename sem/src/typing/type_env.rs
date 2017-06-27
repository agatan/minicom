use std::collections::HashMap;

use syntax::ast::Type as AstType;

use super::typed_ast::Type;
use errors::Error;

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

    pub fn convert(&self, atyp: &AstType) -> Result<Type, Error> {
        match *atyp {
            AstType::Primary(ref name) => self.get(name),
            AstType::Ref(ref inner) => self.convert(inner).map(Type::new_ref),
        }
    }
}
