use std::rc::Rc;
use std::cell::RefCell;

use basis::sourcemap::Span;
use syntax::ast::Operator;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Ref(Box<Type>),
    Var(Rc<RefCell<Option<Type>>>),
}

impl Type {
    pub fn newvar() -> Type {
        Type::Var(Rc::new(RefCell::new(None)))
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: String,
    pub typ: Type,
    pub value: Node,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub ret: Type,
    pub body: Node,
}

#[derive(Debug, Clone)]
pub enum DeclKind {
    Def(Box<Def>),
    Let(Box<Let>),
}
