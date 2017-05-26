pub mod ir;
mod typing;

use std::collections::HashMap;

use self::ir::{Node, NodeKind, Type, Let};
use ast::{NodeId, Node as AstNode, NodeKind as AstNodeKind};
pub use self::typing::TypeMap;

error_chain! {
    types {
        Error, ErrorKind, ResultExt, Result;
    }

    errors {
        InvalidTypeUnification(t1: Type, t2: Type) {
            description("invalid type unification")
            display("cannot unify types: {:?} and {:?}", t1, t2)
        }

        CannotInfer(x: NodeId) {
            description("cannot infer type")
            display("cannot infer type: {:?}", x)
        }
    }
}

#[derive(Debug)]
pub struct Checker {
    pub typemap: TypeMap,
    venv: HashMap<String, Type>,
}

impl Checker {
    pub fn check(nodes: &[AstNode]) -> Result<(Vec<Node>, TypeMap)> {
        let mut chk = Self::new();
        let mut results = Vec::with_capacity(nodes.len());
        for node in nodes {
            chk.transform_node(node).map(|node| results.push(node))?;
        }
        Ok((results, chk.typemap))
    }

    pub fn new() -> Self {
        Checker {
            typemap: TypeMap::new(),
            venv: HashMap::new(),
        }
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
            AstNodeKind::Add(ref l, ref r) => {
                let left = self.transform_node(l)?;
                let right = self.transform_node(r)?;
                let lty = self.typemap.get(l.id);
                let rty = self.typemap.get(r.id);
                if lty != rty {
                    return Err(ErrorKind::InvalidTypeUnification(lty, rty).into());
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
                    return Err(ErrorKind::InvalidTypeUnification(lty, rty).into());
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
                    return Err(ErrorKind::InvalidTypeUnification(lty, rty).into());
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
                    return Err(ErrorKind::InvalidTypeUnification(lty, rty).into());
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
                let name = l.name.clone();
                let value = self.transform_node(&l.value)?;
                assert!(l.typ.is_none());
                self.venv.insert(name.clone(), value.typ.clone());
                Ok(Node::new(NodeKind::Let(Box::new(Let {
                                 name: name,
                                 typ: value.typ.clone(),
                                 value: value,
                             })),
                             Type::Int))
            }
        }
    }
}
