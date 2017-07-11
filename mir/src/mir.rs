use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: HashMap<String, Decl>,
    pub main: Option<Box<Def>>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            decls: HashMap::new(),
            main: None,
        }
    }

    pub fn define(&mut self, name: String, decl: Decl) {
        self.decls.insert(name, decl);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,
    Ref(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Unit,
    Int(i32),
    Float(f64),
    Bool(bool),
    Ident(Var),
    Call(String, Vec<Node>),
    AddInt(Box<Node>, Box<Node>),
    SubInt(Box<Node>, Box<Node>),
    MulInt(Box<Node>, Box<Node>),
    DivInt(Box<Node>, Box<Node>),
    AddFloat(Box<Node>, Box<Node>),
    SubFloat(Box<Node>, Box<Node>),
    MulFloat(Box<Node>, Box<Node>),
    DivFloat(Box<Node>, Box<Node>),
    Not(Box<Node>),
    Eq(Box<Node>, Box<Node>),
    LE(Box<Node>, Box<Node>),
    LT(Box<Node>, Box<Node>),
    Block(Vec<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    While(Box<Node>, Box<Node>),

    Let(Box<Let>),
    AssignGlobal(Var, Box<Node>),

    Ref(Box<Node>),
    Deref(Box<Node>),
    Assign(Box<Node>, Box<Node>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub value: Node,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Node,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Def(Box<Def>),
    Let(Box<Let>),
}
