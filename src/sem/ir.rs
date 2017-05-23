use std::convert::From;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Variable(TypeVariable),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable {
    pub id: u32,
}

impl TypeVariable {
    pub fn new(id: u32) -> Self {
        TypeVariable {
            id: id,
        }
    }
}

impl From<TypeVariable> for Type {
    fn from(x: TypeVariable) -> Type {
        Type::Variable(x)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub typ: Type,
}
