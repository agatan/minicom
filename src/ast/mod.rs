use std::convert::From;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Expr(Expr),
    Stmt(Stmt),
}

impl Node {
    pub fn get_id(&self) -> NodeId {
        match *self {
            Node::Expr(ref e) => e.id,
            Node::Stmt(ref s) => s.id,
        }
    }
}

impl From<Expr> for Node {
    fn from(e: Expr) -> Node {
        Node::Expr(e)
    }
}

impl From<Stmt> for Node {
    fn from(e: Stmt) -> Node {
        Node::Stmt(e)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Let(Box<Let>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub typ: Option<Type>,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
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
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind) -> Self {
        Expr {
            id: id,
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Int,
    Float,
    Hole,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Type { kind: kind }
    }
}
