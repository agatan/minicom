use std::collections::HashMap;

use sem::{ErrorKind, Result};
use sem::ir::{Type, Node, NodeKind};
use ast::{NodeId, Node as AstNode, NodeKind as AstNodeKind};

#[derive(Debug)]
pub struct TypeMap {
    table: HashMap<NodeId, Type>,
}

impl TypeMap {
    pub fn new() -> Self {
        TypeMap { table: HashMap::new() }
    }

    pub fn insert(&mut self, id: NodeId, v: Type) {
        self.table.insert(id, v);
    }

    pub fn get(&self, id: NodeId) -> Type {
        self.table.get(&id).cloned().expect("all ast node should be define in type map")
    }
}

#[derive(Debug)]
pub struct Checker {
    pub typemap: TypeMap,
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
        Checker { typemap: TypeMap::new() }
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
            AstNodeKind::Let(_) => unimplemented!(),
        }
    }
}
