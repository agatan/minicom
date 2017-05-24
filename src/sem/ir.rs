use std::convert::From;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
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

impl Expr {
    pub fn new(kind: ExprKind, typ: Type) -> Self {
        Expr {
            kind: kind,
            typ: typ,
        }
    }
}
