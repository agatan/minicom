use basis::errors::Error as BasisError;
use mir;

use Result;
use syntax::ast::Operator;
use typing::{Module, Node, NodeKind, Decl, DeclKind, Def, Let, Type};

#[derive(Debug)]
pub struct Transform {
    program: mir::Program,
}

impl Transform {
    pub fn new() -> Self {
        Transform { program: mir::Program::new() }
    }

    fn typ(&mut self, typ: Type) -> Result<mir::Type> {
        match typ {
            Type::Unit => Ok(mir::Type::Unit),
            Type::Int => Ok(mir::Type::Int),
            Type::Bool => Ok(mir::Type::Bool),
            Type::Float => Ok(mir::Type::Float),
            Type::Ref(inner) => {
                let inner = Box::new(self.typ(*inner)?);
                Ok(mir::Type::Ref(inner))
            }
            Type::Fun(_) => {
                Err(BasisError::from_error(
                    "function type value is not implemented yet",
                ))
            }
            Type::Var(inner) => {
                match *inner.borrow() {
                    None => {
                        Err(BasisError::from_error(
                            "typing is failed: undetermined type variable",
                        ))
                    }
                    Some(ref typ) => self.typ(typ.clone()),
                }
            }
        }
    }

    fn infix_node(&mut self, lhs: Node, op: Operator, rhs: Node) -> Result<mir::NodeKind> {
        let lhs = self.node(lhs).map(Box::new)?;
        let rhs = self.node(rhs).map(Box::new)?;
        let kind = match op {
            Operator::Add => mir::NodeKind::Add(lhs, rhs),
            Operator::Sub => mir::NodeKind::Sub(lhs, rhs),
            Operator::Mul => mir::NodeKind::Mul(lhs, rhs),
            Operator::Div => mir::NodeKind::Div(lhs, rhs),
            Operator::Eq => mir::NodeKind::Eq(lhs, rhs),
            Operator::LE => mir::NodeKind::LE(lhs, rhs),
            Operator::LT => mir::NodeKind::LT(lhs, rhs),
            Operator::GE => mir::NodeKind::LT(rhs, lhs),
            Operator::GT => mir::NodeKind::LE(rhs, lhs),
            Operator::Neq => mir::NodeKind::Not(Box::new(mir::Node {
                typ: mir::Type::Bool,
                kind: mir::NodeKind::Eq(lhs, rhs),
            })),
        };
        Ok(kind)
    }

    fn node(&mut self, node: Node) -> Result<mir::Node> {
        let Node { kind, span, typ } = node;
        let typ = self.typ(typ).map_err(|err| err.assign_span(span))?;
        let kind = match kind {
            NodeKind::Unit => mir::NodeKind::Unit,
            NodeKind::Int(n) => mir::NodeKind::Int(n),
            NodeKind::Float(n) => mir::NodeKind::Float(n),
            NodeKind::Bool(n) => mir::NodeKind::Bool(n),
            NodeKind::Ident(name) => mir::NodeKind::Ident(mir::Var { name: name }),
            NodeKind::Call(fname, args) => {
                mir::NodeKind::Call(
                    fname,
                    try!(args.into_iter().map(|arg| self.node(arg)).collect()),
                )
            }
            NodeKind::Block(nodes, last) => {
                let mut nodes: Vec<_> = try!(nodes.into_iter().map(|n| self.node(n)).collect());
                nodes.push(self.node(*last)?);
                mir::NodeKind::Block(nodes)
            }
            NodeKind::If(cond, then, els) => {
                let cond = self.node(*cond).map(Box::new)?;
                let then = self.node(*then).map(Box::new)?;
                let els = match els {
                    Some(els) => Some(self.node(*els).map(Box::new)?),
                    None => None,
                };
                mir::NodeKind::If(cond, then, els)
            }
            NodeKind::While(cond, body) => {
                let cond = self.node(*cond).map(Box::new)?;
                let body = self.node(*body).map(Box::new)?;
                mir::NodeKind::While(cond, body)
            }
            NodeKind::Ref(inner) => mir::NodeKind::Ref(self.node(*inner).map(Box::new)?),
            NodeKind::Deref(inner) => mir::NodeKind::Deref(self.node(*inner).map(Box::new)?),
            NodeKind::Assign(to, from) => {
                mir::NodeKind::Assign(
                    self.node(*to).map(Box::new)?,
                    self.node(*from).map(Box::new)?,
                )
            }
            NodeKind::Let(let_) => {
                let let_ = *let_;
                let value = self.node(let_.value)?;
                mir::NodeKind::Let(Box::new(mir::Let {
                    name: let_.name,
                    value: value,
                }))
            }
            NodeKind::Infix(lhs, op, rhs) => self.infix_node(*lhs, op, *rhs)?,
        };
        Ok(mir::Node {
            kind: kind,
            typ: typ,
        })
    }

    fn def(&mut self, def: Def) -> Result<mir::Def> {
        let body = self.node(def.body)?;
        let params = def.params
            .into_iter()
            .map(|p| {
                let typ = self.typ(p.typ)?;
                Ok(mir::Param {
                    name: p.name,
                    typ: typ,
                })
            })
            .collect::<Result<_>>()?;
        let ret = self.typ(def.ret)?;
        Ok(mir::Def {
            name: def.name,
            ret: ret,
            params: params,
            body: body,
        })
    }

    fn register_decl(&mut self, name: String, decl: Decl) -> Result<()> {
        match decl.kind {
            DeclKind::Def(def) => {
                let is_main = def.is_main();
                let def = self.def(*def)?;
                if is_main {
                    self.program.main = Some(Box::new(def));
                } else {
                    self.program.define(name, mir::Decl::Def(Box::new(def)));
                }
            }
            DeclKind::Let(let_) => {
                let l = *let_;
                let Let {
                    name: let_name,
                    value,
                    ..
                } = l;
                let node = self.node(value)?;
                let l = mir::Let {
                    name: let_name,
                    value: node,
                };
                self.program.define(name, mir::Decl::Let(Box::new(l)));
            }
        }
        Ok(())
    }

    pub fn module(&mut self, module: Module) -> Result<()> {
        for (decl_name, decl) in module.decls.into_iter() {
            self.register_decl(decl_name, decl)?;
        }
        Ok(())
    }
}

pub fn typed_ast_to_mir(module: Module) -> Result<mir::Program> {
    let mut transform = Transform::new();
    transform.module(module)?;
    Ok(transform.program)
}
