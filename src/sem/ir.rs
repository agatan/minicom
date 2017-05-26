#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Int(i64),
    Float(f64),
    Ident(IdentId),
    AddInt(Box<Node>, Box<Node>),
    SubInt(Box<Node>, Box<Node>),
    MulInt(Box<Node>, Box<Node>),
    DivInt(Box<Node>, Box<Node>),
    AddFloat(Box<Node>, Box<Node>),
    SubFloat(Box<Node>, Box<Node>),
    MulFloat(Box<Node>, Box<Node>),
    DivFloat(Box<Node>, Box<Node>),
    Print(Box<Node>),

    Let(Box<Let>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentId(usize);

impl IdentId {
    pub fn new(x: usize) -> Self {
        IdentId(x)
    }

    pub fn to_usize(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub typ: Type,
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub typ: Type,
}

impl Node {
    pub fn new(kind: NodeKind, typ: Type) -> Self {
        Node {
            kind: kind,
            typ: typ,
        }
    }
}
