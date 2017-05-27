use std::collections::HashMap;

pub mod ir;
mod typing;
mod venv;
mod tyenv;

use self::ir::{Program, Node, NodeKind, Type, Let};
use ast::{Node as AstNode, NodeKind as AstNodeKind};
pub use self::typing::TypeMap;
use self::venv::VariableEnv;
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
    }
}

#[derive(Debug)]
pub struct Context {
    typemap: TypeMap,
    venv: VariableEnv,
    tyenv: TypeEnv,
}

impl Context {
    pub fn new() -> Self {
        Context {
            typemap: TypeMap::new(),
            venv: VariableEnv::new(),
            tyenv: TypeEnv::new(),
        }
    }

    pub fn transform(&mut self, nodes: &[AstNode]) -> Result<Program> {
        self.collect_types(nodes)?;
        let nodes = nodes.iter().map(|n| self.transform_node(n)).collect::<Result<Vec<_>>>()?;
        Ok(Program {
            functions: HashMap::new(),
            toplevel: nodes,
        })
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
                    self.venv.insert_function(def.name.clone(), args, ret);
                }
                _ => continue,
            }
        }
        Ok(())
    }

    pub fn transform_node(&mut self, node: &AstNode) -> Result<Node> {
        let e = self.transform_node_(node)?;
        self.typemap.insert(node.id, e.typ.clone());
        Ok(e)
    }

    fn transform_node_(&mut self, node: &AstNode) -> Result<Node> {
        match node.kind {
            AstNodeKind::Int(n) => Ok(Node::new(NodeKind::Int(n), Type::Int)),
            AstNodeKind::Float(f) => Ok(Node::new(NodeKind::Float(f), Type::Float)),
            AstNodeKind::Ident(ref name) => {
                match self.venv.get_var(name) {
                    None => bail!(ErrorKind::Undefined(name.clone())),
                    Some((id, level, typ)) => {
                        Ok(Node::new(NodeKind::Ident(id, level), typ.clone()))
                    }
                }
            }
            AstNodeKind::Add(ref l, ref r) => {
                let left = self.transform_node(l)?;
                let right = self.transform_node(r)?;
                let lty = self.typemap.get(l.id);
                let rty = self.typemap.get(r.id);
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
                let lty = self.typemap.get(l.id);
                let rty = self.typemap.get(r.id);
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
                let lty = self.typemap.get(l.id);
                let rty = self.typemap.get(r.id);
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
                let id = self.venv.insert(l.name.clone(), value.typ.clone());
                Ok(Node::new(NodeKind::Let(Box::new(Let {
                                 id: id,
                                 typ: value.typ.clone(),
                                 value: value,
                             })),
                             Type::Unit))
            }
            AstNodeKind::Assign(ref var, ref value) => {
                let (id, level, typ) = self.venv
                    .get_var(var)
                    .ok_or(Error::from(ErrorKind::Undefined(var.clone())))?;
                let value = self.transform_node(value)?;
                if typ != value.typ {
                    bail!(ErrorKind::InvalidTypeUnification(typ, value.typ));
                }
                Ok(Node::new(NodeKind::Assign(id, level, Box::new(value)), Type::Unit))
            }
            AstNodeKind::Def(_) => unimplemented!(),
        }
    }
}
