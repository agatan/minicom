use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub functions: HashMap<IdentId, Function>,
    pub toplevel: Vec<Node>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            functions: HashMap::new(),
            toplevel: Vec::new(),
        }
    }

    pub fn define_function(&mut self, id: IdentId, f: Function) {
        self.functions.insert(id, f);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Unit,
}

#[derive(Debug)]
pub struct Function {
    pub args: Vec<(IdentId, Type)>,
    pub ret_typ: Type,
    pub body: Vec<Node>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Unit,
    Int(i64),
    Float(f64),
    Ident(IdentId, Level),
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
    Assign(IdentId, Level, Box<Node>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentId(u32);

impl IdentId {
    pub fn new(x: u32) -> Self {
        IdentId(x)
    }

    pub fn to_u32(&self) -> u32 {
        self.0
    }
}

pub type Level = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub id: IdentId,
    pub typ: Type,
    pub value: Node,
}
