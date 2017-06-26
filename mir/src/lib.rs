extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;
extern crate minicom_typing as typing;

mod mir;

pub use mir::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
