use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
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
    Int(i64),
    Float(f64),
    Ident(Level<Var>),
    Call(Level<u32>, Vec<Node>),
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
    Assign(Level<Var>, Box<Node>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub index: u32,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub id: u32,
    pub typ: Type,
    pub value: Node,
}

#[derive(Debug)]
pub struct Function {
    pub env: LocalEnv,
    pub args: Vec<Type>,
    pub ret_typ: Type,
    pub body: Vec<Node>,
}

#[derive(Debug)]
pub struct LocalEnv {
    locals: Vec<String>,
    functions: Vec<String>,
    table: HashMap<String, Entry>,
}

impl LocalEnv {
    pub fn new() -> Self {
        LocalEnv {
            locals: Vec::new(),
            functions: Vec::new(),
            table: HashMap::new(),
        }
    }

    pub fn define_local(&mut self, name: String, typ: Type) -> u32 {
        let n = self.locals.len() as u32;
        self.locals.push(name.clone());
        self.table.insert(name,
                          Entry::Var(Var {
                              index: n,
                              typ: typ,
                          }));
        n
    }

    pub fn declare_function(&mut self, name: String, args: Vec<Type>, ret: Type) {
        let n = self.functions.len() as u32;
        self.functions.push(name.clone());
        self.table.insert(name, Entry::ExternFunction(n, args, ret));
    }

    pub fn define_function(&mut self, name: String, f: Function) {
        let n = match self.table.get(&name) {
            Some(&Entry::ExternFunction(n, _, _)) => n,
            Some(_) => unimplemented!(), // FIXME(agatan): duplicated identifiers
            None => {
                let n = self.functions.len() as u32;
                self.functions.push(name.clone());
                n
            }
        };
        self.table.insert(name, Entry::Function(n, f));
    }

    pub fn get_local(&self, name: &str) -> Option<Var> {
        self.table.get(name).and_then(|entry| match *entry {
            Entry::Var(ref var) => Some(var.clone()),
            _ => None,
        })
    }

    pub fn get_function_info(&self, name: &str) -> Option<FunctionInfo> {
        self.table.get(name).and_then(|entry| match *entry {
            Entry::Function(index, ref f) => {
                Some(FunctionInfo {
                    index: index,
                    args: f.args.clone(),
                    ret: f.ret_typ.clone(),
                })
            }
            Entry::ExternFunction(index, ref args, ref ret) => {
                Some(FunctionInfo {
                    index: index,
                    args: args.clone(),
                    ret: ret.clone(),
                })
            }
            Entry::Var(_) => None,
        })
    }
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub index: u32,
    pub args: Vec<Type>,
    pub ret: Type,
}

#[derive(Debug)]
pub enum Entry {
    Var(Var),
    Function(u32, Function),
    ExternFunction(u32, Vec<Type>, Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Level<T> {
    pub value: T,
    pub level: u32,
}
