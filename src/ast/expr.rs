use ast::types::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Parens(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub typ: Type,
}

impl Expr {
    pub fn new(id: ExprId, kind: ExprKind) -> Self {
        Expr {
            id: id,
            kind: kind,
            typ: Type::hole(),
        }
    }

    pub fn with_typ(id: ExprId, kind: ExprKind, typ: Type) -> Self {
        Expr {
            id: id,
            kind: kind,
            typ: typ,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(u32);

impl ExprId {
    pub fn new(id: u32) -> Self {
        ExprId(id)
    }

    pub fn to_u32(self) -> u32 {
        self.0
    }
}
