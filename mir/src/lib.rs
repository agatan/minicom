extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;
extern crate minicom_typing as typing;

#[macro_use]
extern crate error_chain;

use basis::errors::Error as BasisError;

mod mir;

pub use mir::*;

pub mod errors {
    error_chain! {
        errors { }
    }
}

pub type Result<T> = ::std::result::Result<T, BasisError<self::errors::Error>>;

pub fn compile_typed_module(_module: typing::Module) -> Result<Program> {
    unimplemented!()
}
