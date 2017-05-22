use std::rc::Rc;
use std::convert::From;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Int,
    Hole,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: Rc<TypeKind>,
}

impl Type {
    pub fn int() -> Self {
        TypeKind::Int.into()
    }

    pub fn hole() -> Self {
        TypeKind::Hole.into()
    }

    pub fn is(&self, kind: &TypeKind) -> bool {
        *self.kind == *kind
    }
}

impl From<TypeKind> for Type {
    fn from(typ: TypeKind) -> Self {
        Type {
            kind: Rc::new(typ),
        }
    }
}
