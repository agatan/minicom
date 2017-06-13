use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};

pub mod ir;
mod typing;
mod alpha;
mod tyenv;
pub mod infer;

use basis::pos::{Span, Spanned};
use basis::errors::Error as BasisError;
use syntax::ast::{self, Toplevel, ToplevelKind, Node as AstNode, NodeKind as AstNodeKind, Operator};

use self::ir::*;
pub use self::typing::TypeMap;
// use self::venv::VariableEnv;
use self::tyenv::TypeEnv;
use self::alpha::Alpha;

mod errors {
    error_chain! {
        errors { }
    }
}

pub use self::errors::{Error, ErrorKind};

pub type Result<T> = ::std::result::Result<T, BasisError<Error>>;

macro_rules! bail_with {
    ($span:expr, $kind:expr) => {
        return Err(BasisError::span($span, $kind.into()));
    };
    ($span:expr, $fmt:expr, $($args:tt)+) => {
        return Err(BasisError::span($span, ErrorKind::Msg(format!($fmt, $($args)+))));
    };
}

#[derive(Debug)]
pub struct Context {
    typemap: Rc<RefCell<TypeMap>>,
    rootenv: LocalEnv,
    envchain: Vec<LocalEnv>,
    tyenv: Rc<TypeEnv>,
    function_id: u32,
    forward_decls: HashMap<String, FunctionInfo>,
    program: Program,
}

impl Context {
    pub fn new() -> Self {
        Context {
            typemap: Rc::new(RefCell::new(TypeMap::new())),
            rootenv: LocalEnv::new(),
            envchain: Vec::new(),
            tyenv: Rc::new(TypeEnv::new()),
            function_id: 0,
            forward_decls: HashMap::new(),
            program: Program::new(),
        }
    }

    pub fn transform(&mut self, nodes: Vec<Spanned<Toplevel>>) -> Result<Program> {
        let mut alp = Alpha::new();
        debug!("before: {:?}", nodes);
        let nodes = nodes
            .into_iter()
            .map(|n| n.map(|n| alp.apply_toplevel(n)))
            .collect::<Vec<_>>();
        debug!("alpha: {:?}", nodes);
        self.collect_types(&nodes)?;
        self.program.toplevels = nodes
            .iter()
            .map(|n| self.transform_toplevel(n))
            .collect::<Result<_>>()?;
        Ok(::std::mem::replace(&mut self.program, Program::new()))
    }

    fn enter_scope(&mut self) -> Scoped {
        let e = LocalEnv::new();
        self.envchain.push(e);
        Scoped(self)
    }

    fn current_env(&mut self) -> &mut LocalEnv {
        match self.envchain.last_mut() {
            Some(env) => env,
            None => &mut self.rootenv,
        }
    }

    fn next_function_id(&mut self) -> u32 {
        let n = self.function_id;
        self.function_id += 1;
        n
    }

    fn get_var(&self, name: &str) -> Option<(Var, VarKind)> {
        for env in self.envchain.iter().rev() {
            if let Some(var) = env.get_local(name) {
                return Some((var, VarKind::Local));
            }
        }
        self.rootenv
            .get_local(name)
            .map(|var| (var, VarKind::Global))
    }

    fn get_function_info(&self, name: &str) -> Option<FunctionInfo> {
        if let Some(fd) = self.forward_decls.get(name) {
            return Some(FunctionInfo {
                            index: fd.index,
                            args: fd.args.clone(),
                            ret: fd.ret.clone(),
                        });
        }
        for env in self.envchain.iter().rev() {
            if let Some(finfo) = env.get_function_info(name) {
                return Some(finfo);
            }
        }
        self.rootenv.get_function_info(name)
    }

    fn unify_type(&self, expected: &Type, actual: &Type, span: Span) -> Result<()> {
        if expected == actual {
            Ok(())
        } else {
            bail_with!(span,
                       "mismatched types: expected {:?}, found {:?}",
                       expected,
                       actual);
        }
    }

    fn collect_types(&mut self, nodes: &[Spanned<Toplevel>]) -> Result<()> {
        for node in nodes {
            match node.value.kind {
                ToplevelKind::Def(ref def) => {
                    let ret = def.ret
                        .as_ref()
                        .map(|typ| self.tyenv.get(&typ.name))
                        .unwrap_or(Ok(Type::Unit))
                        .map_err(|err| BasisError::span(node.span, err))?;
                    let args = def.args
                        .iter()
                        .map(|&(ref name, ref typ)| {
                                 self.tyenv
                                     .get(&typ.name)
                                     .map(|ty| (name.clone(), ty))
                                     .map_err(|err| BasisError::span(node.span, err))
                             })
                        .collect::<Result<Vec<_>>>()?;
                    let id = self.next_function_id();
                    let fd = FunctionInfo {
                        index: id,
                        args: args,
                        ret: ret,
                    };
                    self.forward_decls.insert(def.name.clone(), fd);
                }
                _ => continue,
            }
        }
        Ok(())
    }

    pub fn transform_toplevel(&mut self, toplevel: &Spanned<Toplevel>) -> Result<Node> {
        match toplevel.value.kind {
            ToplevelKind::Def(ref def) => {
                let function = self.transform_def(def, toplevel.span)?;
                self.define_function(def.name.clone(), function);
                Ok(Node::new(NodeKind::Unit, Type::Unit))
            }
            ToplevelKind::Let(ref l) => {
                let value = self.transform_node(&l.value)?;
                if let Some(ref spec_typ) = l.typ {
                    let typ = self.tyenv
                        .get(&spec_typ.value.name)
                        .map_err(|err| BasisError::span(spec_typ.span, err))?;
                    self.unify_type(&typ, &value.typ, toplevel.span)?;
                }
                self.define_var(l.name.clone(), value.typ.clone());
                Ok(Node::new(NodeKind::AssignGlobal(Var {
                                                        name: l.name.clone(),
                                                        typ: value.typ.clone(),
                                                    },
                                                    Box::new(value)),
                             Type::Unit))
            }
            ToplevelKind::Expr(ref e) => self.transform_node(e),
        }
    }

    pub fn transform_node(&mut self, node: &Spanned<AstNode>) -> Result<Node> {
        let e = self.transform_node_(node)?;
        self.typemap
            .borrow_mut()
            .insert(node.value.id, e.typ.clone());
        Ok(e)
    }

    fn transform_node_(&mut self, node: &Spanned<AstNode>) -> Result<Node> {
        match node.value.kind {
            AstNodeKind::Unit => Ok(Node::new(NodeKind::Unit, Type::Unit)),
            AstNodeKind::Int(n) => Ok(Node::new(NodeKind::Int(n), Type::Int)),
            AstNodeKind::Float(f) => Ok(Node::new(NodeKind::Float(f), Type::Float)),
            AstNodeKind::Bool(b) => Ok(Node::new(NodeKind::Bool(b), Type::Bool)),
            AstNodeKind::Ident(ref name) => {
                match self.get_var(name) {
                    None => {
                        bail_with!(node.span, "undefined identifier {:?}", name);
                    }
                    Some((lv_var, var_kind)) => {
                        let typ = lv_var.typ.clone();
                        match var_kind {
                            VarKind::Global => Ok(Node::new(NodeKind::GlobalIdent(lv_var), typ)),
                            VarKind::Local => Ok(Node::new(NodeKind::Ident(lv_var), typ)),
                        }
                    }
                }
            }
            AstNodeKind::Call(ref fname, ref args) => {
                let finfo = match self.get_function_info(fname) {
                    None => bail_with!(node.span, "undefined function {:?}", fname),
                    Some(r) => r,
                };
                let args = args.iter()
                    .map(|n| self.transform_node(n))
                    .collect::<Result<Vec<_>>>()?;
                if args.len() != finfo.args.len() {
                    bail_with!(node.span,
                               "invalid number of arguments for {:?}: expected {}, but given {}",
                               fname,
                               finfo.args.len(),
                               args.len());
                }
                for (&(_, ref required), given) in
                    finfo.args.iter().zip(args.iter().map(|n| &n.typ)) {

                    self.unify_type(required, given, node.span)?;
                }
                Ok(Node::new(NodeKind::Call(fname.clone(), args), finfo.ret.clone()))
            }
            AstNodeKind::Infix(ref l, op, ref r) => {
                let left = Box::new(self.transform_node(l)?);
                let right = Box::new(self.transform_node(r)?);
                let lty = left.typ.clone();
                let rty = right.typ.clone();
                let (kind, typ) = match op {
                    Operator::Add => {
                        match (lty, rty) {
                            (Type::Int, Type::Int) => (NodeKind::AddInt(left, right), Type::Int),
                            (Type::Float, Type::Float) => {
                                (NodeKind::AddFloat(left, right), Type::Float)
                            }
                            (lty, rty) => {
                                bail_with!(node.span,
                                           "mismatched types: expected int or float, found {:?} and {:?}",
                                           lty,
                                           rty);
                            }
                        }
                    }
                    Operator::Sub => {
                        match (lty, rty) {
                            (Type::Int, Type::Int) => (NodeKind::SubInt(left, right), Type::Int),
                            (Type::Float, Type::Float) => {
                                (NodeKind::SubFloat(left, right), Type::Float)
                            }
                            (lty, rty) => {
                                bail_with!(node.span,
                                           "mismatched types: expected int or float, found {:?} and {:?}",
                                           lty,
                                           rty);
                            }
                        }
                    }
                    Operator::Mul => {
                        match (lty, rty) {
                            (Type::Int, Type::Int) => (NodeKind::MulInt(left, right), Type::Int),
                            (Type::Float, Type::Float) => {
                                (NodeKind::MulFloat(left, right), Type::Float)
                            }
                            (lty, rty) => {
                                bail_with!(node.span,
                                           "mismatched types: expected int or float, found {:?} and {:?}",
                                           lty,
                                           rty);
                            }
                        }
                    }
                    Operator::Div => {
                        match (lty, rty) {
                            (Type::Int, Type::Int) => (NodeKind::DivInt(left, right), Type::Int),
                            (Type::Float, Type::Float) => {
                                (NodeKind::DivFloat(left, right), Type::Float)
                            }
                            (lty, rty) => {
                                bail_with!(node.span,
                                           "mismatched types: expected int or float, found {:?} and {:?}",
                                           lty,
                                           rty);
                            }
                        }
                    }
                    cmp => {
                        self.unify_type(&lty, &rty, node.span)?;
                        let kind = match cmp {
                            Operator::Eq => NodeKind::Eq(left, right),
                            Operator::Neq => {
                                NodeKind::Not(Box::new(Node::new(NodeKind::Eq(left, right),
                                                                 Type::Bool)))
                            }
                            Operator::LE => NodeKind::LE(left, right),
                            Operator::LT => NodeKind::LT(left, right),
                            Operator::GE => {
                                NodeKind::Not(Box::new(Node::new(NodeKind::LT(left, right),
                                                                 Type::Bool)))
                            }
                            Operator::GT => {
                                NodeKind::Not(Box::new(Node::new(NodeKind::LE(left, right),
                                                                 Type::Bool)))
                            }
                            _ => unreachable!(),
                        };
                        return Ok(Node::new(kind, Type::Bool));
                    }
                };
                Ok(Node::new(kind, typ))
            }
            AstNodeKind::Parens(ref e) => self.transform_node(e),
            AstNodeKind::Print(ref e) => {
                let e = self.transform_node(e)?;
                let ty = e.typ.clone();
                Ok(Node::new(NodeKind::Print(Box::new(e)), ty))
            }
            AstNodeKind::Block(ref nodes) => {
                let nodes = nodes
                    .into_iter()
                    .map(|n| self.transform_node(n))
                    .collect::<Result<Vec<_>>>()?;
                let typ = nodes.last().map(|n| n.typ.clone()).unwrap_or(Type::Unit);
                Ok(Node::new(NodeKind::Block(nodes), typ))
            }
            AstNodeKind::If(ref cond, ref then, ref els) => {
                let cond = self.transform_node(cond)?;
                let then = self.transform_node(then)?;
                let (els, typ) = match *els {
                    None => (None, Type::Unit),
                    Some(ref els) => {
                        let els = self.transform_node(els)?;
                        let typ = els.typ.clone();
                        (Some(Box::new(els)), typ)
                    }
                };
                self.unify_type(&then.typ, &typ, node.span)?;
                Ok(Node::new(NodeKind::If(Box::new(cond), Box::new(then), els), typ))
            }
            AstNodeKind::While(ref cond, ref body) => {
                let cond = self.transform_node(cond)?;
                self.unify_type(&Type::Bool, &cond.typ, node.span)?;
                let body = self.transform_node(body)?;
                let typ = body.typ.clone();
                Ok(Node::new(NodeKind::While(Box::new(cond), Box::new(body)), typ))
            }
            AstNodeKind::Let(ref l) => {
                let value = self.transform_node(&l.value)?;
                if let Some(ref typ) = l.typ {
                    let typ = self.tyenv
                        .get(&typ.value.name)
                        .map_err(|err| BasisError::span(node.span, err))?;
                    self.unify_type(&typ, &value.typ, node.span)?;
                }
                self.define_var(l.name.clone(), value.typ.clone());
                Ok(Node::new(NodeKind::Let(Box::new(Let {
                                                        name: l.name.clone(),
                                                        typ: value.typ.clone(),
                                                        value: value,
                                                    })),
                             Type::Unit))
            }
            AstNodeKind::Assign(ref var, ref value) => {
                let (lv_var, var_kind) = match self.get_var(var) {
                    Some(x) => x,
                    None => bail_with!(node.span, "undefined identifier {:?}", var),
                };
                let typ = lv_var.typ.clone();
                let value = self.transform_node(value)?;
                self.unify_type(&typ, &value.typ, node.span)?;
                match var_kind {
                    VarKind::Global => {
                        Ok(Node::new(NodeKind::AssignGlobal(lv_var, Box::new(value)), Type::Unit))
                    }
                    VarKind::Local => {
                        Ok(Node::new(NodeKind::Assign(lv_var, Box::new(value)), Type::Unit))
                    }
                }
            }
        }
    }

    fn transform_def(&mut self, def: &ast::Def, _span: Span) -> Result<Function> {
        let fd = self.forward_decls
            .get(&def.name)
            .expect("forward declared")
            .clone();
        let mut scoped = self.enter_scope();
        for &(ref name, ref ty) in fd.args.iter() {
            scoped.define_var(name.clone(), ty.clone());
        }
        let body = scoped.transform_node(&def.body)?;
        if fd.ret != body.typ {
            let span = def.body.span;
            bail_with!(span,
                       "invalid type unification: {:?} and {:?}",
                       fd.ret,
                       body.typ);
        }
        let function = Function {
            id: fd.index,
            name: def.name.clone(),
            args: fd.args,
            ret_typ: fd.ret,
            body: Box::new(body),
        };
        Ok(function)
    }

    fn is_root(&self) -> bool {
        self.envchain.is_empty()
    }

    fn define_var(&mut self, name: String, typ: Type) {
        if self.is_root() {
            self.current_env().define_local(name.clone(), typ.clone());
            self.program.define_global(name, typ);
        } else {
            self.current_env().define_local(name.clone(), typ.clone())
        }
    }

    fn define_function(&mut self, name: String, f: Function) {
        self.current_env().define_function(name.clone(), f.clone());
        self.program.define_function(name, f);
    }
}

struct Scoped<'a>(&'a mut Context);

impl<'a> Deref for Scoped<'a> {
    type Target = Context;

    fn deref(&self) -> &Context {
        self.0
    }
}

impl<'a> DerefMut for Scoped<'a> {
    fn deref_mut(&mut self) -> &mut Context {
        self.0
    }
}


impl<'a> Drop for Scoped<'a> {
    fn drop(&mut self) {
        self.0.envchain.pop();
    }
}
