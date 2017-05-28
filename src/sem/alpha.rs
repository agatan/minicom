use std::collections::HashMap;

use ast::*;

#[derive(Debug)]
pub struct Alpha<'a> {
    parent: Option<&'a Alpha<'a>>,
    vars: HashMap<String, usize>,
}

impl<'a> Alpha<'a> {
    pub fn new() -> Self {
        Alpha {
            parent: None,
            vars: HashMap::new(),
        }
    }

    pub fn scope<'p>(&'p self) -> Alpha<'p> {
        Alpha {
            parent: Some(self),
            vars: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String) -> String {
        let n = self.get_dupnum(&name);
        if n == 0 {
            self.vars.insert(name.clone(), 1);
            return name;
        }
        let newname = format!("{}#{}", &name, n);
        self.vars.insert(name, n + 1);
        newname
    }

    pub fn transform(&mut self, name: String) -> String {
        let n = self.get_dupnum(&name);
        if n <= 1 {
            name
        } else {
            format!("{}#{}", name, n - 1)
        }
    }

    fn get_dupnum(&self, name: &str) -> usize {
        if let Some(&n) = self.vars.get(name) {
            return n;
        }
        if let Some(parent) = self.parent {
            return parent.get_dupnum(name);
        }
        0
    }

    pub fn apply(&mut self, mut node: Node) -> Node {
        node.kind = match node.kind {
            NodeKind::Unit => NodeKind::Unit,
            NodeKind::Int(n) => NodeKind::Int(n),
            NodeKind::Float(n) => NodeKind::Float(n),
            NodeKind::Bool(b) => NodeKind::Bool(b),
            NodeKind::Ident(name) => NodeKind::Ident(self.transform(name)),
            NodeKind::Infix(l, op, r) => {
                NodeKind::Infix(Box::new(self.apply(*l)), op, Box::new(self.apply(*r)))
            }
            NodeKind::Parens(e) => NodeKind::Parens(Box::new(self.apply(*e))),
            NodeKind::Print(e) => NodeKind::Print(Box::new(self.apply(*e))),
            NodeKind::Call(name, args) => {
                NodeKind::Call(self.transform(name),
                               args.into_iter().map(|n| self.apply(n)).collect())
            }
            NodeKind::Block(nodes) => {
                let mut scope = self.scope();
                let nodes = nodes.into_iter().map(|n| scope.apply(n)).collect();
                NodeKind::Block(nodes)
            }
            NodeKind::If(cond, then, els) => {
                let cond = self.apply(*cond);
                let then = self.apply(*then); // `then` is always Block, so new scope will be created
                let els = els.map(|n| Box::new(self.apply(*n)));
                NodeKind::If(Box::new(cond), Box::new(then), els)
            }
            NodeKind::While(cond, body) => {
                let cond = self.apply(*cond);
                let body = self.apply(*body);
                NodeKind::While(Box::new(cond), Box::new(body))
            }
            NodeKind::Let(let_) => {
                let mut let_ = *let_;
                let_.value = self.apply(let_.value);
                let_.name = self.define(let_.name);
                NodeKind::Let(Box::new(let_))
            }
            NodeKind::Assign(name, value) => {
                let name = self.transform(name);
                let value = self.apply(*value);
                NodeKind::Assign(name, Box::new(value))
            }
            NodeKind::Def(def) => {
                let mut def = *def;
                def.name = self.define(def.name);
                let mut scoped = self.scope();
                def.args =
                    def.args.into_iter().map(|(name, typ)| (scoped.define(name), typ)).collect();
                def.body = def.body.into_iter().map(|n| scoped.apply(n)).collect();
                NodeKind::Def(Box::new(def))
            }
        };
        node
    }
}
