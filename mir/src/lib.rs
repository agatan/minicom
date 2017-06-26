//! `mir` crate defined MIR (midterm intermediate representation) of `minicom` language.
//! `sem` crate converts AST defined in `syntax` crate into MIR.
//! This crate responds to optimize MIR.

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
