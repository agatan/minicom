#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Ident(String),
    Call(String, Vec<Node>),
    Infix(Box<Node>, Operator, Box<Node>),
    Parens(Box<Node>),
    Block(Vec<Node>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
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
