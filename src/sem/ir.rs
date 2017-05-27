use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub functions: HashMap<FunctionId, Function>,
    pub toplevel: Vec<Node>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            functions: HashMap::new(),
            toplevel: Vec::new(),
        }
    }

    pub fn define_function(&mut self, id: FunctionId, f: Function) {
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
    pub args: Vec<(LocalId, Type)>,
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
    Ident(LocalId, Level),
    Call(FunctionId, Vec<Node>),
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
    Assign(LocalId, Level, Box<Node>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(u32);

impl LocalId {
    pub fn new(x: u32) -> Self {
        LocalId(x)
    }

    pub fn to_u32(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(u32);

impl FunctionId {
    pub fn new(x: u32) -> Self {
        FunctionId(x)
    }

    pub fn to_u32(&self) -> u32 {
        self.0
    }
}

pub type Level = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub id: LocalId,
    pub typ: Type,
    pub value: Node,
}
