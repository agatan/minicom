use std::convert::From;

use basis::sourcemap::{Span, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum ToplevelKind {
    Def(Box<Def>),
    Let(Box<Let>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Toplevel {
    pub span: Span,
    pub kind: ToplevelKind,
}

impl Toplevel {
    pub fn new(span: Span, kind: ToplevelKind) -> Self {
        Toplevel {
            span: span,
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Unit,
    Int(i32),
    Float(f64),
    Bool(bool),
    Ident(String),
    Call(String, Vec<Node>),
    Infix(Box<Node>, Operator, Box<Node>),
    Parens(Box<Node>),
    Block(Vec<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    While(Box<Node>, Box<Node>),
    // ref(1)
    Ref(Box<Node>),
    // @x
    Deref(Box<Node>),
    // x <- 1
    Assign(Box<Node>, Box<Node>),
    Let(Box<Let>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

impl Node {
    pub fn new(span: Span, kind: NodeKind) -> Self {
        Node {
            span: span,
            kind: kind,
        }
    }
}

impl From<Spanned<NodeKind>> for Node {
    fn from(sn: Spanned<NodeKind>) -> Node {
        Node {
            span: sn.span,
            kind: sn.value,
        }
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
    pub value: Node,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub span: Span,
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
    pub body: Node,
}

impl Def {
    pub fn is_main(&self) -> bool {
        self.name == "main"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primary(String),
    Ref(Box<Type>),
}

impl Type {
    pub fn new(name: String) -> Type {
        Type::Primary(name)
    }

    pub fn newref(inner: Type) -> Type {
        Type::Ref(Box::new(inner))
    }
}
