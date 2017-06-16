pub mod ir;
mod typing;
mod alpha;
mod tyenv;
pub mod infer;

use basis::pos::Spanned;
use basis::errors::Error as BasisError;
use syntax::ast::{self, Toplevel, ToplevelKind, NodeId, Node as AstNode, NodeKind as AstNodeKind,
                  Operator};

use self::ir::*;
pub use self::typing::TypeMap;
use self::alpha::Alpha;
use self::infer::Infer;

#[cfg(test)]
mod tests;

mod errors {
    error_chain! {
        errors { }
    }
}

pub use self::errors::{Error, ErrorKind};

pub type Result<T> = ::std::result::Result<T, BasisError<Error>>;

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
        let Spanned { value: node, span } = node;
        let typ = self.inferer
            .gettyp(node.id)
            .map_err(|err| BasisError::span(span, err))?;
        let kind = match node.kind {
            AstNodeKind::Unit => NodeKind::Unit,
            AstNodeKind::Int(n) => NodeKind::Int(n),
            AstNodeKind::Float(n) => NodeKind::Float(n),
            AstNodeKind::Bool(n) => NodeKind::Bool(n),
            AstNodeKind::Ident(name) => {
                let typ = self.inferer
                    .gettyp(node.id)
                    .map_err(|err| BasisError::span(span, err))?;
                NodeKind::Ident(Var {
                                    name: name,
                                    typ: typ.clone(),
                                })
            }
            AstNodeKind::Call(fname, args) => {
                let args = args.into_iter()
                    .map(|arg| self.process_node(arg))
                    .collect::<Result<Vec<_>>>()?;
                NodeKind::Call(fname, args)
            }
            AstNodeKind::Parens(e) => return self.process_node(*e),
            AstNodeKind::Block(inner) => {
                let inner = inner
                    .into_iter()
                    .map(|n| self.process_node(n))
                    .collect::<Result<Vec<_>>>()?;
                NodeKind::Block(inner)
            }
            AstNodeKind::If(cond, then, els) => {
                let cond = self.process_node(*cond)?;
                let then = self.process_node(*then)?;
                let els = match els {
                    None => None,
                    Some(els) => Some(Box::new(self.process_node(*els)?)),
                };
                NodeKind::If(Box::new(cond), Box::new(then), els)
            }
            AstNodeKind::While(cond, body) => {
                let cond = self.process_node(*cond)?;
                let body = self.process_node(*body)?;
                NodeKind::While(Box::new(cond), Box::new(body))
            }
            AstNodeKind::Print(e) => {
                let e = self.process_node(*e)?;
                NodeKind::Print(Box::new(e))
            }
            AstNodeKind::Let(let_) => {
                let let_ = *let_;
                let ast::Let { value, name, .. } = let_;
                let value_typ = self.inferer
                    .gettyp(value.value.id)
                    .map_err(|err| BasisError::span(span, err))?;
                let value = self.process_node(value)?;
                NodeKind::Let(Box::new(Let {
                                           name: name,
                                           typ: value_typ,
                                           value: value,
                                       }))
            }
            AstNodeKind::Assign(name, value) => {
                let value = Box::new(self.process_node(*value)?);
                NodeKind::Assign(Var {
                                     name: name,
                                     typ: typ.clone(),
                                 },
                                 value)
            }
            AstNodeKind::Infix(lhs, op, rhs) => {
                let lhs = Box::new(self.process_node(*lhs)?);
                let rhs = Box::new(self.process_node(*rhs)?);
                match op {
                    Operator::Add => {
                        if typ == Type::Int {
                            NodeKind::AddInt(lhs, rhs)
                        } else {
                            NodeKind::AddFloat(lhs, rhs)
                        }
                    }
                    Operator::Sub => {
                        if typ == Type::Int {
                            NodeKind::SubInt(lhs, rhs)
                        } else {
                            NodeKind::SubFloat(lhs, rhs)
                        }
                    }
                    Operator::Mul => {
                        if typ == Type::Int {
                            NodeKind::MulInt(lhs, rhs)
                        } else {
                            NodeKind::MulFloat(lhs, rhs)
                        }
                    }
                    Operator::Div => {
                        if typ == Type::Int {
                            NodeKind::DivInt(lhs, rhs)
                        } else {
                            NodeKind::DivFloat(lhs, rhs)
                        }
                    }
                    Operator::Eq => NodeKind::Eq(lhs, rhs),
                    Operator::Neq => {
                        NodeKind::Not(Box::new(Node::new(NodeKind::Eq(lhs, rhs), Type::Bool)))
                    }
                    Operator::LE => NodeKind::LE(lhs, rhs),
                    Operator::LT => NodeKind::LT(lhs, rhs),
                    Operator::GE => {
                        NodeKind::Not(Box::new(Node::new(NodeKind::LT(lhs, rhs), Type::Bool)))
                    }
                    Operator::GT => {
                        NodeKind::Not(Box::new(Node::new(NodeKind::LE(lhs, rhs), Type::Bool)))
                    }
                }
            }
        };
        Ok(Node::new(kind, typ))
    }

    fn process_global_def(&mut self, id: NodeId, def: Spanned<ast::Def>) -> Result<()> {
        let Spanned { value: def, .. } = def;
        let (arg_types, ret) = self.inferer.get_toplevel_function(id);
        let body = self.process_node(def.body)?;
        let args = def.args
            .into_iter()
            .map(|(name, _)| name)
            .zip(arg_types)
            .collect();
        let function = Function {
            name: def.name.clone(),
            args: args,
            ret_typ: ret,
            body: Box::new(body),
        };
        self.program.define_function(def.name, function);
        Ok(())
    }

    fn process_global_let(&mut self, let_: Spanned<ast::Let>) -> Result<()> {
        let Spanned { value: let_, span } = let_;
        let ast::Let { name, value, .. } = let_;
        let typ = self.inferer
            .gettyp(value.value.id)
            .map_err(|err| BasisError::span(span, err))?;
        let irnode = self.process_node(value)?;
        self.program.define_global(name.clone(), typ.clone());
        let var = Var {
            name: name,
            typ: typ,
        };
        let assignment = Node::new(NodeKind::AssignGlobal(var, Box::new(irnode)), Type::Unit);
        self.program.append_toplevel(assignment);
        Ok(())
    }

    fn process_toplevel(&mut self, toplevel: Spanned<Toplevel>) -> Result<()> {
        match toplevel.value.kind {
            ToplevelKind::Def(def) => {
                self.process_global_def(toplevel.value.id, Spanned::span(toplevel.span, *def))
            }
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
}
