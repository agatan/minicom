use ast::types::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Parens(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub typ: ArcTyp,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr {
            kind: kind,
            typ: ArcTyp::hole(),
        }
    }

    pub fn with_typ(kind: ExprKind, typ: ArcTyp) -> Self {
        Expr {
            kind: kind,
            typ: typ,
        }
    }
}
