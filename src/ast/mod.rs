mod types;
pub use self::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }
}

pub trait Node {
    fn get_id(&self) -> NodeId;
}

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
    pub id: NodeId,
    pub kind: ExprKind,
    pub typ: Type,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind) -> Self {
        Expr {
            id: id,
            kind: kind,
            typ: Type::hole(),
        }
    }

    pub fn with_typ(id: NodeId, kind: ExprKind, typ: Type) -> Self {
        Expr {
            id: id,
            kind: kind,
            typ: typ,
        }
    }
}

impl Node for Expr {
    fn get_id(&self) -> NodeId {
        self.id
    }
}
