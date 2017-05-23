use std::collections::HashMap;

use sem::ir::{Type, TypeVariable};

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

    pub fn lookup(&self, x: TypeVariable) -> Type {
        if let Some(ty) = self.table.get(&x) {
            return ty.clone();
        }
        self.parent.map(|p| p.lookup(x)).unwrap_or(x.into())
    }

    pub fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Int => Type::Int,
            Type::Variable(x) => {
                let u = self.lookup(x);
                match u {
                    Type::Variable(y) if y == x => u,
                    _ => self.apply(u),
                }
            }
        }
    }
}

#[test]
fn test_subst() {
    let mut parent = Substitution::new();
    parent.insert(TypeVariable::new(0), Type::Int);
    parent.insert(TypeVariable::new(1), Type::from(TypeVariable::new(0)));
    assert_eq!(parent.apply(Type::from(TypeVariable::new(1))), Type::Int);
    {
        let mut child = parent.scoped();
        child.insert(TypeVariable::new(3), Type::Int);
        child.insert(TypeVariable::new(4), Type::from(TypeVariable::new(1)));
        assert_eq!(child.apply(Type::from(TypeVariable::new(3))), Type::Int);
        assert_eq!(child.apply(Type::from(TypeVariable::new(4))), Type::Int);
    }
}
