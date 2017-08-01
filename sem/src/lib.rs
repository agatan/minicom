#[macro_use]
extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;
extern crate minicom_mir as mir;

#[macro_use]
extern crate error_chain;

use basis::errors::Error as BasisError;

pub mod typing;
pub mod alpha;
mod transform;
pub use transform::typed_ast_to_mir;

use typing::{Module, Node, NodeKind, Decl, DeclKind};

pub mod errors {
    error_chain! {
        errors { }
    }
}

pub type Result<T> = ::std::result::Result<T, BasisError<self::errors::Error>>;

pub fn ast_to_mir(
    module_name: String,
    program: Vec<syntax::ast::Toplevel>,
) -> Result<mir::Program> {
    let module = typing::typecheck(module_name, program)?;
    let module = alpha::transform(module);
    typed_ast_to_mir(module)
}
