#[macro_use]
extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;
#[macro_use]
extern crate error_chain;

use basis::errors::Error as BasisError;
use syntax::ast::Toplevel;

mod typed_ast;
pub use typed_ast::*;
mod type_env;
mod infer;
pub use infer::Infer;
mod deref;
pub use deref::*;

pub mod errors {
    error_chain! {
        errors { }
    }
}

pub type Result<T> = ::std::result::Result<T, BasisError<self::errors::Error>>;

pub fn typecheck(module_name: String, program: Vec<Toplevel>) -> Result<Module> {
    let mut infer = Infer::new();
    let mut module = infer.process(module_name, program)?;
    deref(&mut module)?;
    Ok(module)
}
