use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use basis::sourcemap::Span;
use syntax::ast::Operator;

use errors::Error;

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub decls: HashMap<String, Decl>,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Unit,
    Int(i32),
    Float(f64),
    Bool(bool),
    Ident(String),
    Call(String, Vec<Node>),
    Infix(Box<Node>, Operator, Box<Node>),
    /// statements and last expression
    Block(Vec<Node>, Box<Node>),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Float,
    Bool,
    Ref(Box<Type>),
    Fun(Rc<(Vec<Type>, Type)>),
    Var(Rc<RefCell<Option<Type>>>),
}

impl Type {
    pub fn new_var() -> Type {
        Type::Var(Rc::new(RefCell::new(None)))
    }

    pub fn new_fun(params: Vec<Type>, ret: Type) -> Self {
        Type::Fun(Rc::new((params, ret)))
    }

    pub fn new_ref(inner: Type) -> Self {
        Type::Ref(Box::new(inner))
    }

    pub fn deref(&self) -> Result<Type, Error> {
        match *self {
            Type::Unit => Ok(Type::Unit),
            Type::Int => Ok(Type::Int),
            Type::Float => Ok(Type::Float),
            Type::Bool => Ok(Type::Bool),
            Type::Ref(ref inner) => {
                let inner = inner.deref()?;
                Ok(Type::new_ref(inner))
            }
            Type::Fun(ref f) => {
                let params = f.0
                    .iter()
                    .map(|t| Type::deref(t))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret = f.1.clone().deref()?;
                Ok(Type::new_fun(params, ret))
            }
            Type::Var(ref inner) => {
                match inner.borrow_mut().as_mut() {
                    None => bail!("type annotation required"),
                    Some(inner) => {
                        let typ = inner.deref()?;
                        *inner = typ.clone();
                        Ok(typ)
                    }
                }
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Unit => f.write_str("()"),
            Type::Int => f.write_str("Int"),
            Type::Float => f.write_str("Float"),
            Type::Bool => f.write_str("Bool"),
            Type::Ref(ref inner) => write!(f, "Ref[{}]", inner),
            Type::Fun(ref fun) => {
                let params = fun.0
                    .iter()
                    .map(|p| p.to_string())
                    .fold("".to_string(), |acc, param| if acc.is_empty() {
                        param
                    } else {
                        format!("{}, {}", acc, param)
                    });
                let ret = &fun.1;
                write!(f, "({}) => {}", params, ret)
            }
            Type::Var(ref inner) => {
                match inner.borrow().as_ref() {
                    None => f.write_str("_"),
                    Some(ty) => write!(f, "{}", ty),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: String,
    pub typ: Type,
    pub value: Node,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Node,
}

#[derive(Debug, Clone)]
pub enum DeclKind {
    Def(Box<Def>),
    Let(Box<Let>),
}

#[derive(Debug, Clone)]
pub struct Decl {
    pub kind: DeclKind,
    pub declare_typ: Type,
    pub span: Span,
}

impl Decl {
    pub fn name(&self) -> &str {
        match self.kind {
            DeclKind::Def(ref def) => &def.name,
            DeclKind::Let(ref let_) => &let_.name,
        }
    }
}
