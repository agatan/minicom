use mir;

use Result;
use typing::{Module, Node, NodeKind, Decl, DeclKind, Def, Let, Type};

#[derive(Debug)]
pub struct Transform {
    program: mir::Program,
}

impl Transform {
    pub fn new() -> Self {
        Transform { program: mir::Program::new() }
    }

    fn typ(&mut self, typ: Type) -> mir::Type {
        unimplemented!()
    }

    fn node(&mut self, node: Node) -> mir::Node {
        unimplemented!()
    }

    fn def(&mut self, def: Def) -> mir::Def {
        let body = self.node(def.body);
        let params = def.params
            .into_iter()
            .map(|p| {
                let typ = self.typ(p.typ);
                mir::Param {
                    name: p.name,
                    typ: typ,
                }
            })
            .collect();
        let ret = self.typ(def.ret);
        mir::Def {
            name: def.name,
            ret: ret,
            params: params,
            body: body,
        }
    }

    fn register_decl(&mut self, name: String, decl: Decl) {
        match decl.kind {
            DeclKind::Def(def) => {
                let is_main = def.is_main();
                let def = self.def(*def);
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
                let node = self.node(value);
                let l = mir::Let {
                    name: let_name,
                    value: node,
                };
                self.program.define(name, mir::Decl::Let(Box::new(l)));
            }
        }
    }

    pub fn module(&mut self, module: Module) {
        for (decl_name, decl) in module.decls.into_iter() {
            self.register_decl(decl_name, decl);
        }
    }
}

pub fn typed_ast_to_mir(module: Module) -> mir::Program {
    let mut transform = Transform::new();
    transform.module(module);
    transform.program
}
