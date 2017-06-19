use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use super::Error;

#[derive(Debug)]
pub struct Program {
    pub entries: HashMap<String, Entry>,
    pub toplevels: Vec<Node>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            entries: HashMap::new(),
            toplevels: Vec::new(),
        }
    }

    pub fn append_toplevel(&mut self, node: Node) {
        self.toplevels.push(node)
    }

    pub fn define_function(&mut self, name: String, f: Function) {
        self.entries.insert(name, Entry::Function(f));
    }

    pub fn define_global(&mut self, name: String, typ: Type) {
        self.entries
            .insert(name.clone(),
                    Entry::Var(Var {
                                   name: name,
                                   typ: typ,
                               }));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,
    Ref(Box<Type>),
    Var(Rc<RefCell<Option<Type>>>),
}

impl Type {
    pub fn newvar() -> Type {
        Type::Var(Rc::new(RefCell::new(None)))
    }

    pub fn deref(self) -> Result<Type, Error> {
        match self {
            Type::Var(inner) => {
                match inner.borrow_mut().as_mut() {
                    None => bail!("type annotation needed"),
                    Some(inner) => inner.clone().deref(),
                }
            }
            Type::Ref(inner) => {
                let inner = inner.deref()?;
                Ok(Type::Ref(Box::new(inner)))
            }
            typ => Ok(typ),
        }
    }
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
    Print(Box<Node>),
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
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub typ: Type,
    pub value: Node,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret_typ: Type,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub args: Vec<(String, Type)>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub enum Entry {
    Var(Var),
    Function(Function),
}
