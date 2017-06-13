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
use self::infer::Infer;

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
    inferer: Infer,
    program: Program,
}

impl Context {
    pub fn new() -> Self {
        Context {
            inferer: Infer::new(),
            program: Program::new(),
        }
    }

    fn process_node(&mut self, node: Spanned<AstNode>) -> Result<Node> {
        unimplemented!()
    }

    fn process_global_def(&mut self, def: Spanned<ast::Def>) -> Result<()> {
        unimplemented!()
    }

    fn process_global_let(&mut self, let_: Spanned<ast::Let>) -> Result<()> {
        let Spanned { value: let_, span } = let_;
        let irnode = self.process_node(let_.value)?;
        let typ = self.inferer.gettyp(let_.value.value.id);
        self.program.define_global(let_.name.clone(), typ.clone());
        let var = Var {
            name: let_.name,
            typ: typ,
        };
        let assignment = Node::new(NodeKind::AssignGlobal(var, Box::new(irnode)), Type::Unit);
        self.program.append_toplevel(assignment);
        Ok(())
    }

    fn process_toplevel(&mut self, toplevel: Spanned<Toplevel>) -> Result<()> {
        match toplevel.value.kind {
            ToplevelKind::Def(def) => self.process_global_def(Spanned::span(toplevel.span, *def)),
            ToplevelKind::Let(let_) => self.process_global_let(Spanned::span(toplevel.span, *let_)),
            ToplevelKind::Expr(expr) => {
                let node = self.process_node(*expr)?;
                self.program.append_toplevel(node);
                Ok(())
            }
        }
    }

    /// `check_and_transform` handle type checking and inference by `Infer`, and transform AST
    /// nodes to semantical IR (`Program`).
    /// All transformation functions in `Context` does not do any type checks because `Infer`
    /// already checks them.
    pub fn check_and_transform(&mut self, program: Vec<Spanned<Toplevel>>) -> Result<Program> {
        self.inferer.infer_program(&program)?;
        let mut alp = Alpha::new();
        let program = program
            .into_iter()
            .map(|n| n.map(|n| alp.apply_toplevel(n)))
            .collect::<Vec<_>>();
        for toplevel in program {
            self.process_toplevel(toplevel)?;
        }
        Ok(::std::mem::replace(&mut self.program, Program::new()))
    }

    // fn transform_node_(&mut self, node: &Spanned<AstNode>) -> Result<Node> {
    //     match node.value.kind {
    //         AstNodeKind::Unit => Ok(Node::new(NodeKind::Unit, Type::Unit)),
    //         AstNodeKind::Int(n) => Ok(Node::new(NodeKind::Int(n), Type::Int)),
    //         AstNodeKind::Float(f) => Ok(Node::new(NodeKind::Float(f), Type::Float)),
    //         AstNodeKind::Bool(b) => Ok(Node::new(NodeKind::Bool(b), Type::Bool)),
    //         AstNodeKind::Ident(ref name) => {
    //             match self.get_var(name) {
    //                 None => {
    //                     bail_with!(node.span, "undefined identifier {:?}", name);
    //                 }
    //                 Some((lv_var, var_kind)) => {
    //                     let typ = lv_var.typ.clone();
    //                     match var_kind {
    //                         VarKind::Global => Ok(Node::new(NodeKind::GlobalIdent(lv_var), typ)),
    //                         VarKind::Local => Ok(Node::new(NodeKind::Ident(lv_var), typ)),
    //                     }
    //                 }
    //             }
    //         }
    //         AstNodeKind::Call(ref fname, ref args) => {
    //             let finfo = match self.get_function_info(fname) {
    //                 None => bail_with!(node.span, "undefined function {:?}", fname),
    //                 Some(r) => r,
    //             };
    //             let args = args.iter()
    //                 .map(|n| self.transform_node(n))
    //                 .collect::<Result<Vec<_>>>()?;
    //             if args.len() != finfo.args.len() {
    //                 bail_with!(node.span,
    //                            "invalid number of arguments for {:?}: expected {}, but given {}",
    //                            fname,
    //                            finfo.args.len(),
    //                            args.len());
    //             }
    //             for (&(_, ref required), given) in
    //                 finfo.args.iter().zip(args.iter().map(|n| &n.typ)) {
    //
    //                 self.unify_type(required, given, node.span)?;
    //             }
    //             Ok(Node::new(NodeKind::Call(fname.clone(), args), finfo.ret.clone()))
    //         }
    //         AstNodeKind::Infix(ref l, op, ref r) => {
    //             let left = Box::new(self.transform_node(l)?);
    //             let right = Box::new(self.transform_node(r)?);
    //             let lty = left.typ.clone();
    //             let rty = right.typ.clone();
    //             let (kind, typ) = match op {
    //                 Operator::Add => {
    //                     match (lty, rty) {
    //                         (Type::Int, Type::Int) => (NodeKind::AddInt(left, right), Type::Int),
    //                         (Type::Float, Type::Float) => {
    //                             (NodeKind::AddFloat(left, right), Type::Float)
    //                         }
    //                         (lty, rty) => {
    //                             bail_with!(node.span,
    //                                        "mismatched types: expected int or float, found {:?} and {:?}",
    //                                        lty,
    //                                        rty);
    //                         }
    //                     }
    //                 }
    //                 Operator::Sub => {
    //                     match (lty, rty) {
    //                         (Type::Int, Type::Int) => (NodeKind::SubInt(left, right), Type::Int),
    //                         (Type::Float, Type::Float) => {
    //                             (NodeKind::SubFloat(left, right), Type::Float)
    //                         }
    //                         (lty, rty) => {
    //                             bail_with!(node.span,
    //                                        "mismatched types: expected int or float, found {:?} and {:?}",
    //                                        lty,
    //                                        rty);
    //                         }
    //                     }
    //                 }
    //                 Operator::Mul => {
    //                     match (lty, rty) {
    //                         (Type::Int, Type::Int) => (NodeKind::MulInt(left, right), Type::Int),
    //                         (Type::Float, Type::Float) => {
    //                             (NodeKind::MulFloat(left, right), Type::Float)
    //                         }
    //                         (lty, rty) => {
    //                             bail_with!(node.span,
    //                                        "mismatched types: expected int or float, found {:?} and {:?}",
    //                                        lty,
    //                                        rty);
    //                         }
    //                     }
    //                 }
    //                 Operator::Div => {
    //                     match (lty, rty) {
    //                         (Type::Int, Type::Int) => (NodeKind::DivInt(left, right), Type::Int),
    //                         (Type::Float, Type::Float) => {
    //                             (NodeKind::DivFloat(left, right), Type::Float)
    //                         }
    //                         (lty, rty) => {
    //                             bail_with!(node.span,
    //                                        "mismatched types: expected int or float, found {:?} and {:?}",
    //                                        lty,
    //                                        rty);
    //                         }
    //                     }
    //                 }
    //                 cmp => {
    //                     self.unify_type(&lty, &rty, node.span)?;
    //                     let kind = match cmp {
    //                         Operator::Eq => NodeKind::Eq(left, right),
    //                         Operator::Neq => {
    //                             NodeKind::Not(Box::new(Node::new(NodeKind::Eq(left, right),
    //                                                              Type::Bool)))
    //                         }
    //                         Operator::LE => NodeKind::LE(left, right),
    //                         Operator::LT => NodeKind::LT(left, right),
    //                         Operator::GE => {
    //                             NodeKind::Not(Box::new(Node::new(NodeKind::LT(left, right),
    //                                                              Type::Bool)))
    //                         }
    //                         Operator::GT => {
    //                             NodeKind::Not(Box::new(Node::new(NodeKind::LE(left, right),
    //                                                              Type::Bool)))
    //                         }
    //                         _ => unreachable!(),
    //                     };
    //                     return Ok(Node::new(kind, Type::Bool));
    //                 }
    //             };
    //             Ok(Node::new(kind, typ))
    //         }
    //         AstNodeKind::Parens(ref e) => self.transform_node(e),
    //         AstNodeKind::Print(ref e) => {
    //             let e = self.transform_node(e)?;
    //             let ty = e.typ.clone();
    //             Ok(Node::new(NodeKind::Print(Box::new(e)), ty))
    //         }
    //         AstNodeKind::Block(ref nodes) => {
    //             let nodes = nodes
    //                 .into_iter()
    //                 .map(|n| self.transform_node(n))
    //                 .collect::<Result<Vec<_>>>()?;
    //             let typ = nodes.last().map(|n| n.typ.clone()).unwrap_or(Type::Unit);
    //             Ok(Node::new(NodeKind::Block(nodes), typ))
    //         }
    //         AstNodeKind::If(ref cond, ref then, ref els) => {
    //             let cond = self.transform_node(cond)?;
    //             let then = self.transform_node(then)?;
    //             let (els, typ) = match *els {
    //                 None => (None, Type::Unit),
    //                 Some(ref els) => {
    //                     let els = self.transform_node(els)?;
    //                     let typ = els.typ.clone();
    //                     (Some(Box::new(els)), typ)
    //                 }
    //             };
    //             self.unify_type(&then.typ, &typ, node.span)?;
    //             Ok(Node::new(NodeKind::If(Box::new(cond), Box::new(then), els), typ))
    //         }
    //         AstNodeKind::While(ref cond, ref body) => {
    //             let cond = self.transform_node(cond)?;
    //             self.unify_type(&Type::Bool, &cond.typ, node.span)?;
    //             let body = self.transform_node(body)?;
    //             let typ = body.typ.clone();
    //             Ok(Node::new(NodeKind::While(Box::new(cond), Box::new(body)), typ))
    //         }
    //         AstNodeKind::Let(ref l) => {
    //             let value = self.transform_node(&l.value)?;
    //             if let Some(ref typ) = l.typ {
    //                 let typ = self.tyenv
    //                     .get(&typ.value.name)
    //                     .map_err(|err| BasisError::span(node.span, err))?;
    //                 self.unify_type(&typ, &value.typ, node.span)?;
    //             }
    //             self.define_var(l.name.clone(), value.typ.clone());
    //             Ok(Node::new(NodeKind::Let(Box::new(Let {
    //                                                     name: l.name.clone(),
    //                                                     typ: value.typ.clone(),
    //                                                     value: value,
    //                                                 })),
    //                          Type::Unit))
    //         }
    //         AstNodeKind::Assign(ref var, ref value) => {
    //             let (lv_var, var_kind) = match self.get_var(var) {
    //                 Some(x) => x,
    //                 None => bail_with!(node.span, "undefined identifier {:?}", var),
    //             };
    //             let typ = lv_var.typ.clone();
    //             let value = self.transform_node(value)?;
    //             self.unify_type(&typ, &value.typ, node.span)?;
    //             match var_kind {
    //                 VarKind::Global => {
    //                     Ok(Node::new(NodeKind::AssignGlobal(lv_var, Box::new(value)), Type::Unit))
    //                 }
    //                 VarKind::Local => {
    //                     Ok(Node::new(NodeKind::Assign(lv_var, Box::new(value)), Type::Unit))
    //                 }
    //             }
    //         }
    //     }
    // }
    //
    // pub fn transform_node(&mut self, node: &Spanned<AstNode>) -> Result<Node> {
    //     let e = self.transform_node_(node)?;
    //     self.typemap
    //         .borrow_mut()
    //         .insert(node.value.id, e.typ.clone());
    //     Ok(e)
    // }
    //
    // fn transform_def(&mut self, def: &ast::Def, _span: Span) -> Result<Function> {
    //     let fd = self.forward_decls
    //         .get(&def.name)
    //         .expect("forward declared")
    //         .clone();
    //     let mut scoped = self.enter_scope();
    //     for &(ref name, ref ty) in fd.args.iter() {
    //         scoped.define_var(name.clone(), ty.clone());
    //     }
    //     let body = scoped.transform_node(&def.body)?;
    //     if fd.ret != body.typ {
    //         let span = def.body.span;
    //         bail_with!(span,
    //                    "invalid type unification: {:?} and {:?}",
    //                    fd.ret,
    //                    body.typ);
    //     }
    //     let function = Function {
    //         id: fd.index,
    //         name: def.name.clone(),
    //         args: fd.args,
    //         ret_typ: fd.ret,
    //         body: Box::new(body),
    //     };
    //     Ok(function)
    // }
    //
    // fn is_root(&self) -> bool {
    //     self.envchain.is_empty()
    // }
    //
    // fn define_var(&mut self, name: String, typ: Type) {
    //     if self.is_root() {
    //         self.current_env().define_local(name.clone(), typ.clone());
    //         self.program.define_global(name, typ);
    //     } else {
    //         self.current_env().define_local(name.clone(), typ.clone())
    //     }
    // }
    //
    // fn define_function(&mut self, name: String, f: Function) {
    //     self.current_env().define_function(name.clone(), f.clone());
    //     self.program.define_function(name, f);
    // }
    //
    // pub fn transform(&mut self, nodes: Vec<Spanned<Toplevel>>) -> Result<Program> {
    //     let mut alp = Alpha::new();
    //     debug!("before: {:?}", nodes);
    //     let nodes = nodes
    //         .into_iter()
    //         .map(|n| n.map(|n| alp.apply_toplevel(n)))
    //         .collect::<Vec<_>>();
    //     debug!("alpha: {:?}", nodes);
    //     self.collect_types(&nodes)?;
    //     self.program.toplevels = nodes
    //         .iter()
    //         .map(|n| self.transform_toplevel(n))
    //         .collect::<Result<_>>()?;
    //     Ok(::std::mem::replace(&mut self.program, Program::new()))
    // }
    //
    // pub fn transform_toplevel(&mut self, toplevel: &Spanned<Toplevel>) -> Result<Node> {
    //     match toplevel.value.kind {
    //         ToplevelKind::Def(ref def) => {
    //             let function = self.transform_def(def, toplevel.span)?;
    //             self.define_function(def.name.clone(), function);
    //             Ok(Node::new(NodeKind::Unit, Type::Unit))
    //         }
    //         ToplevelKind::Let(ref l) => {
    //             let value = self.transform_node(&l.value)?;
    //             if let Some(ref spec_typ) = l.typ {
    //                 let typ = self.tyenv
    //                     .get(&spec_typ.value.name)
    //                     .map_err(|err| BasisError::span(spec_typ.span, err))?;
    //                 self.unify_type(&typ, &value.typ, toplevel.span)?;
    //             }
    //             self.define_var(l.name.clone(), value.typ.clone());
    //             Ok(Node::new(NodeKind::AssignGlobal(Var {
    //                                                     name: l.name.clone(),
    //                                                     typ: value.typ.clone(),
    //                                                 },
    //                                                 Box::new(value)),
    //                          Type::Unit))
    //         }
    //         ToplevelKind::Expr(ref e) => self.transform_node(e),
    //     }
    // }
}
