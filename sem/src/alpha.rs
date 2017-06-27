//! `alpha` module applies alpha-transform to typed AST (`typing::typed_ast`).

use std::collections::HashMap;

use typing::{Module, Node, NodeKind, Let, Decl, DeclKind, Def};

#[derive(Debug)]
struct AlphaTrans {
    scopes: Vec<HashMap<String, String>>,
    var_id: usize,
}

impl AlphaTrans {
    fn new() -> Self {
        AlphaTrans {
            scopes: vec![HashMap::new()],
            var_id: 0,
        }
    }

    fn scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn current(&mut self) -> &mut HashMap<String, String> {
        self.scopes.last_mut().unwrap()
    }

    fn new_var(&mut self, name: &str) -> String {
        let new = format!("{}${}", name, self.var_id);
        self.var_id += 1;
        new
    }

    fn register(&mut self, name: String) -> String {
        let new = self.new_var(&name);
        self.current().insert(name, new.clone());
        new
    }

    fn resolve(&mut self, name: &str) -> String {
        for scope in self.scopes.iter().rev() {
            if let Some(resolved) = scope.get(name) {
                return resolved.clone();
            }
        }
        unreachable!()
    }

    fn process_let(&mut self, let_: Let) -> Let {
        let Let { name, typ, value } = let_;
        let value = self.process_node(value);
        let new_name = self.register(name);
        Let {
            name: new_name,
            typ: typ,
            value: value,
        }
    }

    pub fn process_node(&mut self, mut node: Node) -> Node {
        node.kind = match node.kind {
            NodeKind::Unit |
            NodeKind::Int(_) |
            NodeKind::Float(_) |
            NodeKind::Bool(_) => node.kind,
            NodeKind::Ident(name) => {
                let new_name = self.resolve(&name);
                NodeKind::Ident(new_name)
            }
            NodeKind::Call(fun_name, args) => {
                let fun_name = self.resolve(&fun_name);
                let args = args.into_iter()
                    .map(|node| self.process_node(node))
                    .collect();
                NodeKind::Call(fun_name, args)
            }
            NodeKind::Infix(lhs, op, rhs) => {
                let lhs = self.process_node(*lhs);
                let rhs = self.process_node(*rhs);
                NodeKind::Infix(Box::new(lhs), op, Box::new(rhs))
            }
            NodeKind::Block(stmts, last) => {
                self.scope();
                let stmts = stmts
                    .into_iter()
                    .map(|node| self.process_node(node))
                    .collect();
                let last = self.process_node(*last);
                self.pop();
                NodeKind::Block(stmts, Box::new(last))
            }
            NodeKind::If(cond, then, els) => {
                let cond = self.process_node(*cond);
                let then = self.process_node(*then);
                let els = els.map(|node| Box::new(self.process_node(*node)));
                NodeKind::If(Box::new(cond), Box::new(then), els)
            }
            NodeKind::While(cond, body) => {
                let cond = self.process_node(*cond);
                let body = self.process_node(*body);
                NodeKind::While(Box::new(cond), Box::new(body))
            }
            NodeKind::Ref(e) => NodeKind::Ref(Box::new(self.process_node(*e))),
            NodeKind::Deref(e) => NodeKind::Deref(Box::new(self.process_node(*e))),
            NodeKind::Assign(to, value) => {
                let to = self.process_node(*to);
                let value = self.process_node(*value);
                NodeKind::Assign(Box::new(to), Box::new(value))
            }
            NodeKind::Let(let_) => NodeKind::Let(Box::new(self.process_let(*let_))),
        };
        node
    }

    pub fn process_decl(&mut self, mut decl: Decl) -> Decl {
        decl.kind = match decl.kind {
            DeclKind::Def(def) => {
                let def = *def;
                let Def {
                    name,
                    params,
                    ret,
                    body,
                } = def;
                let name = self.register(name);
                self.scope();
                let params = params
                    .into_iter()
                    .map(|mut param| {
                             param.name = self.register(param.name);
                             param
                         })
                    .collect();
                let body = self.process_node(body);
                DeclKind::Def(Box::new(Def {
                                           name: name,
                                           params: params,
                                           ret: ret,
                                           body: body,
                                       }))
            }
            DeclKind::Let(let_) => DeclKind::Let(Box::new(self.process_let(*let_))),
        };
        decl
    }

    pub fn process(&mut self, mut module: Module) -> Module {
        module.decls = module
            .decls
            .into_iter()
            .map(|(_, decl)| {
                     let decl = self.process_decl(decl);
                     (decl.name().to_string(), decl)
                 })
            .collect();
        module
    }
}

pub fn transform(module: Module) -> Module {
    let mut trans = AlphaTrans::new();
    trans.process(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use typing::*;
    use basis::sourcemap::NSPAN;

    fn unit() -> Node {
        Node {
            kind: NodeKind::Unit,
            typ: Type::Unit,
            span: NSPAN,
        }
    }

    #[test]
    fn test_let() {
        let let_ = Decl {
            kind: DeclKind::Let(Box::new(Let {
                                             name: "x".to_string(),
                                             typ: Type::Unit,
                                             value: unit(),
                                         })),
            declare_typ: Type::Unit,
            span: NSPAN,
        };
        let mut trans = AlphaTrans::new();
        let result = trans.process_decl(let_);
        assert_eq!(result.name(), "x$0");
    }
}
