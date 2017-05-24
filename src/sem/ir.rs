#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    AddInt(Box<Expr>, Box<Expr>),
    SubInt(Box<Expr>, Box<Expr>),
    MulInt(Box<Expr>, Box<Expr>),
    DivInt(Box<Expr>, Box<Expr>),
    AddFloat(Box<Expr>, Box<Expr>),
    SubFloat(Box<Expr>, Box<Expr>),
    MulFloat(Box<Expr>, Box<Expr>),
    DivFloat(Box<Expr>, Box<Expr>),
    Print(Box<Expr>),
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
