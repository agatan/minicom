use std::rc::Rc;
use std::convert::From;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Int,
    Hole,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    kind: Rc<TypeKind>,
}

impl Type {
    pub fn int() -> Self {
        TypeKind::Int.into()
    }

    pub fn hole() -> Self {
        TypeKind::Hole.into()
    }
}

impl From<TypeKind> for Type {
    fn from(typ: TypeKind) -> Self {
        Type {
            kind: Rc::new(typ),
        }
    }
}
