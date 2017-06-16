use basis::pos::Spanned;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

impl NodeId {
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelKind {
    Def(Box<Def>),
    Let(Box<Let>),
    Expr(Box<Spanned<Node>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Toplevel {
    pub id: NodeId,
    pub kind: ToplevelKind,
}

impl Toplevel {
    pub fn new(id: NodeId, kind: ToplevelKind) -> Self {
        Toplevel { id: id, kind: kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Unit,
    Int(i32),
    Float(f64),
    Bool(bool),
    Ident(String),
    Call(String, Vec<Spanned<Node>>),
    Infix(Box<Spanned<Node>>, Operator, Box<Spanned<Node>>),
    Parens(Box<Spanned<Node>>),
    Block(Vec<Spanned<Node>>),
    If(Box<Spanned<Node>>, Box<Spanned<Node>>, Option<Box<Spanned<Node>>>),
    While(Box<Spanned<Node>>, Box<Spanned<Node>>),
    // FIXME(agatan): temporary builtin command
    Print(Box<Spanned<Node>>),
    // ref(1)
    Ref(Box<Spanned<Node>>),
    // @x
    Deref(Box<Spanned<Node>>),
    // x <- 1
    Assign(Box<Spanned<Node>>, Box<Spanned<Node>>),
    Let(Box<Let>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
}

impl Node {
    pub fn new(id: NodeId, kind: NodeKind) -> Self {
        Node { id: id, kind: kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    LE,
    LT,
    GE,
    GT,
}

impl Operator {
    pub fn is_arithmetic(&self) -> bool {
        use self::Operator::*;
        match *self {
            Add | Sub | Mul | Div => true,
            Eq | Neq | LE | LT | GE | GT => false,
        }
    }
    pub fn is_compare(&self) -> bool {
        use self::Operator::*;
        match *self {
            Eq | Neq | LE | LT | GE | GT => true,
            Add | Sub | Mul | Div => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub typ: Option<Spanned<Type>>,
    pub value: Spanned<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret: Option<Type>,
    pub body: Spanned<Node>,
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
