#[macro_use]
extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;
#[macro_use]
extern crate error_chain;

use basis::errors::Error as BasisError;

pub mod typed_ast;
pub mod type_env;
pub mod infer;
pub mod deref;

pub mod errors {
    error_chain! {
        errors { }
    }
}

pub type Result<T> = ::std::result::Result<T, BasisError<self::errors::Error>>;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
