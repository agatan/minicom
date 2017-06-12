use std::collections::HashMap;

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
    GlobalIdent(Var),
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
    Assign(Var, Box<Node>),
    AssignGlobal(Var, Box<Node>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarKind {
    Global,
    Local,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: String,
    pub typ: Type,
    pub value: Node,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: u32,
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret_typ: Type,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct LocalEnv {
    table: HashMap<String, Entry>,
}

impl LocalEnv {
    pub fn new() -> Self {
        LocalEnv { table: HashMap::new() }
    }

    pub fn define_local(&mut self, name: String, typ: Type) {
        self.table
            .insert(name.clone(),
                    Entry::Var(Var {
                                   name: name,
                                   typ: typ,
                               }));
    }

    pub fn define_function(&mut self, name: String, f: Function) {
        self.table.insert(name, Entry::Function(f));
    }

    pub fn get_local(&self, name: &str) -> Option<Var> {
        self.table
            .get(name)
            .and_then(|entry| match *entry {
                          Entry::Var(ref var) => Some(var.clone()),
                          _ => None,
                      })
    }

    pub fn get_function_info(&self, name: &str) -> Option<FunctionInfo> {
        self.table
            .get(name)
            .and_then(|entry| match *entry {
                          Entry::Function(ref f) => {
                              Some(FunctionInfo {
                                       index: f.id,
                                       args: f.args.clone(),
                                       ret: f.ret_typ.clone(),
                                   })
                          }
                          Entry::Var(_) => None,
                      })
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub index: u32,
    pub args: Vec<(String, Type)>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub enum Entry {
    Var(Var),
    Function(Function),
}
