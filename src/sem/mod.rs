use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};

pub mod ir;
mod typing;
// mod venv;
mod tyenv;

use self::ir::*;
use ast::{self, Node as AstNode, NodeKind as AstNodeKind};
pub use self::typing::TypeMap;
// use self::venv::VariableEnv;
use self::tyenv::TypeEnv;

error_chain! {
    types {
        Error, ErrorKind, ResultExt, Result;
    }

    errors {
        Undefined(name: String) {
            description("undefined identifier")
            display("undefined identifier {:?}", name)
        }
        InvalidTypeUnification(t1: Type, t2: Type) {
            description("invalid type unification")
            display("cannot unify types: {:?} and {:?}", t1, t2)
        }
        InvalidArguments(fname: String, required: usize, given: usize) {
            description("invalid arguments")
            display("invalid number of arguments for {}: required {:?}, but given {:?}", fname, required, given)
        }
    }
}

#[derive(Debug)]
pub struct Context {
    typemap: Rc<RefCell<TypeMap>>,
    rootenv: LocalEnv,
    envchain: Vec<LocalEnv>,
    tyenv: Rc<TypeEnv>,
    function_id: u32,
}

impl Context {
    pub fn new() -> Self {
        Context {
            typemap: Rc::new(RefCell::new(TypeMap::new())),
            rootenv: LocalEnv::new(),
            envchain: Vec::new(),
            tyenv: Rc::new(TypeEnv::new()),
            function_id: 0,
        }
    }

    pub fn transform(&mut self, nodes: &[AstNode]) -> Result<Vec<Node>> {
        self.collect_types(nodes)?;
        nodes.iter().map(|n| self.transform_node(n)).collect::<Result<_>>()
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

    fn current_depth(&self) -> u32 {
        self.envchain.len() as u32
    }

    fn is_root(&self, level: u32) -> bool {
        level == self.current_depth()
    }

    fn next_function_id(&mut self) -> u32 {
        let n = self.function_id;
        self.function_id += 1;
        n
    }

    fn get_var(&self, name: &str) -> Option<Level<Var>> {
        for (env, level) in self.envchain.iter().rev().zip(0..) {
            if let Some(var) = env.get_local(name) {
                return Some(Level {
                    value: var,
                    level: level,
                });
            }
        }
        self.rootenv.get_local(name).map(|var| {
            Level {
                value: var,
                level: self.envchain.len() as u32,
            }
        })
    }

    fn get_function_info(&self, name: &str) -> Option<FunctionInfo> {
        for env in self.envchain.iter().rev() {
            if let Some(finfo) = env.get_function_info(name) {
                return Some(finfo);
            }
        }
        self.rootenv.get_function_info(name)
    }

    fn collect_types(&mut self, nodes: &[AstNode]) -> Result<()> {
        for node in nodes {
            match node.kind {
                AstNodeKind::Def(ref def) => {
                    let ret = def.ret
                        .as_ref()
                        .map(|typ| self.tyenv.get(&typ.name))
                        .unwrap_or(Ok(Type::Unit))?;
                    let args = def.args
                        .iter()
                        .map(|&(_, ref typ)| self.tyenv.get(&typ.name))
                        .collect::<Result<Vec<_>>>()?;
                    let id = self.next_function_id();
                    self.current_env()
                        .declare_function(id, def.name.clone(), args, ret);
                }
                _ => continue,
            }
        }
        Ok(())
    }

    pub fn transform_node(&mut self, node: &AstNode) -> Result<Node> {
        let e = self.transform_node_(node)?;
        self.typemap.borrow_mut().insert(node.id, e.typ.clone());
        Ok(e)
    }

    fn transform_node_(&mut self, node: &AstNode) -> Result<Node> {
        match node.kind {
            AstNodeKind::Int(n) => Ok(Node::new(NodeKind::Int(n), Type::Int)),
            AstNodeKind::Float(f) => Ok(Node::new(NodeKind::Float(f), Type::Float)),
            AstNodeKind::Ident(ref name) => {
                match self.get_var(name) {
                    None => bail!(ErrorKind::Undefined(name.clone())),
                    Some(lv_var) => {
                        let typ = lv_var.value.typ.clone();
                        if self.is_root(lv_var.level) {
                            Ok(Node::new(NodeKind::GlobalIdent(lv_var.value), typ))
                        } else {
                            Ok(Node::new(NodeKind::Ident(lv_var), typ))
                        }
                    }
                }
            }
            AstNodeKind::Call(ref fname, ref args) => {
                let finfo = match self.get_function_info(fname) {
                    None => bail!(ErrorKind::Undefined(fname.clone())),
                    Some(r) => r,
                };
                let args = args.iter()
                    .map(|n| self.transform_node(n))
                    .collect::<Result<Vec<_>>>()?;
                if args.len() != finfo.args.len() {
                    bail!(ErrorKind::InvalidArguments(fname.clone(), finfo.args.len(), args.len()));
                }
                for (required, given) in finfo.args.iter().zip(args.iter().map(|n| &n.typ)) {
                    if required != given {
                        bail!(ErrorKind::InvalidTypeUnification(required.clone(), given.clone()));
                    }
                }
                Ok(Node::new(NodeKind::Call(finfo.index, args), finfo.ret.clone()))
            }
            AstNodeKind::Add(ref l, ref r) => {
                let left = self.transform_node(l)?;
                let right = self.transform_node(r)?;
                let lty = left.typ.clone();
                let rty = right.typ.clone();
                if lty != rty {
                    bail!(ErrorKind::InvalidTypeUnification(lty, rty));
                }
                if lty == Type::Int {
                    Ok(Node::new(NodeKind::AddInt(Box::new(left), Box::new(right)), Type::Int))
                } else {
                    Ok(Node::new(NodeKind::AddFloat(Box::new(left), Box::new(right)),
                                 Type::Float))
                }
            }
            AstNodeKind::Sub(ref l, ref r) => {
                let left = self.transform_node(l)?;
                let right = self.transform_node(r)?;
                let lty = left.typ.clone();
                let rty = right.typ.clone();
                if lty != rty {
                    bail!(ErrorKind::InvalidTypeUnification(lty, rty));
                }
                if lty == Type::Int {
                    Ok(Node::new(NodeKind::SubInt(Box::new(left), Box::new(right)), Type::Int))
                } else {
                    Ok(Node::new(NodeKind::SubFloat(Box::new(left), Box::new(right)),
                                 Type::Float))
                }
            }
            AstNodeKind::Mul(ref l, ref r) => {
                let left = self.transform_node(l)?;
                let right = self.transform_node(r)?;
                let lty = left.typ.clone();
                let rty = right.typ.clone();
                if lty != rty {
                    bail!(ErrorKind::InvalidTypeUnification(lty, rty));
                }
                if lty == Type::Int {
                    Ok(Node::new(NodeKind::MulInt(Box::new(left), Box::new(right)), Type::Int))
                } else {
                    Ok(Node::new(NodeKind::MulFloat(Box::new(left), Box::new(right)),
                                 Type::Float))
                }
            }
            AstNodeKind::Div(ref l, ref r) => {
                let left = self.transform_node(l)?;
                let right = self.transform_node(r)?;
                let lty = left.typ.clone();
                let rty = right.typ.clone();
                if lty != rty {
                    bail!(ErrorKind::InvalidTypeUnification(lty, rty));
                }
                if lty == Type::Int {
                    Ok(Node::new(NodeKind::DivInt(Box::new(left), Box::new(right)), Type::Int))
                } else {
                    Ok(Node::new(NodeKind::DivFloat(Box::new(left), Box::new(right)),
                                 Type::Float))
                }
            }
            AstNodeKind::Parens(ref e) => self.transform_node(e),
            AstNodeKind::Print(ref e) => {
                let e = self.transform_node(e)?;
                let ty = e.typ.clone();
                Ok(Node::new(NodeKind::Print(Box::new(e)), ty))
            }
            AstNodeKind::Let(ref l) => {
                let value = self.transform_node(&l.value)?;
                if let Some(ref typ) = l.typ {
                    let typ = self.tyenv.get(&typ.name)?;
                    if typ != value.typ {
                        bail!(ErrorKind::InvalidTypeUnification(typ, value.typ));
                    }
                }
                let id = self.current_env().define_local(l.name.clone(), value.typ.clone());
                Ok(Node::new(NodeKind::Let(Box::new(Let {
                                 id: id,
                                 typ: value.typ.clone(),
                                 value: value,
                             })),
                             Type::Unit))
            }
            AstNodeKind::Assign(ref var, ref value) => {
                let lv_var = self.get_var(var)
                    .ok_or(Error::from(ErrorKind::Undefined(var.clone())))?;
                let typ = lv_var.value.typ.clone();
                let value = self.transform_node(value)?;
                if typ != value.typ {
                    bail!(ErrorKind::InvalidTypeUnification(typ, value.typ));
                }
                if self.is_root(lv_var.level) {
                    Ok(Node::new(NodeKind::AssignGlobal(lv_var.value, Box::new(value)),
                                 Type::Unit))
                } else {
                    Ok(Node::new(NodeKind::Assign(lv_var, Box::new(value)), Type::Unit))
                }
            }
            AstNodeKind::Def(ref def) => {
                let function = self.transform_def(def)?;
                self.define_function(def.name.clone(), function);
                Ok(Node::new(NodeKind::Unit, Type::Unit))
            }
        }
    }

    fn transform_def(&mut self, def: &ast::Def) -> Result<Function> {
        let mut scoped = self.enter_scope();
        let mut args = Vec::new();
        for &(ref name, ref typ) in def.args.iter() {
            let typ = scoped.tyenv.get(&typ.name)?;
            scoped.current_env().define_local(name.clone(), typ.clone());
            args.push(typ);
        }
        let body = def.body
            .iter()
            .map(|node| scoped.transform_node(node))
            .collect::<Result<Vec<_>>>()?;
        let ret_typ = def.ret
            .as_ref()
            .map(|ret| scoped.tyenv.get(&ret.name))
            .unwrap_or(Ok(Type::Unit))?;
        let body_typ = body.last().map(|n| n.typ.clone()).unwrap_or(Type::Unit);
        if ret_typ != body_typ {
            bail!(ErrorKind::InvalidTypeUnification(ret_typ, body_typ));
        }
        let env = scoped.exit_and_pop();
        let function = Function {
            args: args,
            ret_typ: ret_typ,
            body: body,
            env: env,
        };
        Ok(function)
    }

    fn define_function(&mut self, name: String, f: Function) {
        self.current_env().define_function(name, f);
    }
}

struct Scoped<'a>(&'a mut Context);

impl<'a> Scoped<'a> {
    fn exit_and_pop(&mut self) -> LocalEnv {
        self.0.envchain.pop().unwrap()
    }
}

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
