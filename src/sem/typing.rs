use std::collections::HashMap;

use sem::ir::{Type, TypeVariable};
use sem::{Error, ErrorKind, Result};

#[derive(Debug)]
pub struct Substitution<'a> {
    parent: Option<&'a Substitution<'a>>,
    table: HashMap<TypeVariable, Type>,
}

impl<'a> Substitution<'a> {
    pub fn new() -> Self {
        Substitution {
            parent: None,
            table: HashMap::new(),
        }
    }

    pub fn scoped(&'a self) -> Substitution<'a> {
        Substitution {
            parent: Some(self),
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, x: TypeVariable, ty: Type) {
        self.table.insert(x, ty);
    }

    pub fn lookup(&self, x: TypeVariable) -> Result<Type> {
        if let Some(ty) = self.table.get(&x) {
            return Ok(ty.clone());
        }
        if let Some(parent) = self.parent {
            parent.lookup(x)
        } else {
            Err(ErrorKind::NonDeterministicTypeVariable(x).into())
        }
    }

    pub fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Int => Type::Int,
            Type::Variable(x) => {
                match self.lookup(x) {
                    Ok(ty) => self.apply(ty),
                    Err(_) => Type::from(x),
                }
            }
        }
    }

    pub fn deref(&self, ty: Type) -> Result<Type> {
        match ty {
            Type::Int => Ok(Type::Int),
            Type::Variable(x) => {
                self.lookup(x).and_then(|ty| self.deref(ty))
            }
        }
    }
}

#[test]
fn test_subst() {
    let mut parent = Substitution::new();
    parent.insert(TypeVariable::new(0), Type::Int);
    parent.insert(TypeVariable::new(1), Type::from(TypeVariable::new(0)));
    assert_eq!(parent.deref(Type::from(TypeVariable::new(1))).unwrap(), Type::Int);
    {
        let mut child = parent.scoped();
        child.insert(TypeVariable::new(3), Type::Int);
        child.insert(TypeVariable::new(4), Type::from(TypeVariable::new(1)));
        assert_eq!(child.deref(Type::from(TypeVariable::new(3))).unwrap(), Type::Int);
        assert_eq!(child.deref(Type::from(TypeVariable::new(4))).unwrap(), Type::Int);
    }
}
