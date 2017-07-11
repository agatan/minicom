use mir;

use Result;
use typing::{Module, Node, NodeKind, Decl, DeclKind};

#[derive(Debug)]
pub struct Transform {
    program: mir::Program,
}

impl Transform {
    pub fn new() -> Self {
        Transform { program: mir::Program::new() }
    }

    fn decl(&mut self, name: String, decl: Decl) -> Result<()> {
        unimplemented!()
    }

    pub fn module(&mut self, module: Module) -> Result<()> {
        for (decl_name, decl) in module.decls.into_iter() {
            self.decl(decl_name, decl)?;
        }
        Ok(())
    }
}

pub fn typed_ast_to_mir(module: Module) -> Result<mir::Program> {
    let mut transform = Transform::new();
    transform.module(module)?;
    Ok(transform.program)
}
