extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;
extern crate minicom_typing as typing;
extern crate minicom_mir as mir;

#[macro_use]
extern crate error_chain;

use basis::errors::Error as BasisError;

pub mod errors {
    error_chain! {
        errors { }
    }
}

pub type Result<T> = ::std::result::Result<T, BasisError<self::errors::Error>>;

pub fn ast_to_mir(module_name: String,
                  program: Vec<syntax::ast::Toplevel>)
                  -> Result<mir::Program> {
    let module = typing::typecheck(module_name, program)?;
    typed_ast_to_mir(module)
}

pub fn typed_ast_to_mir(module: typing::Module) -> Result<mir::Program> {
    unimplemented!()
}
