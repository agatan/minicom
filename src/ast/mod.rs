#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Int(i64),
    Float(f64),
    Ident(String),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Parens(Box<Node>),
    // FIXME(agatan): temporary builtin command
    Print(Box<Node>),
    Let(Box<Let>),
    Assign(String, Box<Node>),

    Def(Box<Def>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
}

impl Node {
    pub fn new(id: NodeId, kind: NodeKind) -> Self {
        Node {
            id: id,
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub typ: Option<Type>,
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret: Option<Type>,
    pub body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: String,
}

impl Type {
    pub fn new(name: String) -> Type {
        Type { name: name }
    }
}
