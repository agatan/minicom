#![feature(box_syntax)]

extern crate minivm_basis as basis;

#[macro_use]
extern crate quick_error;
extern crate lalrpop_util;

pub mod ast;
mod token;
mod grammar;
mod error;

use std::convert::From;

use basis::pos::Source;
use basis::pos::Spanned;
use basis::errors::Error as BasisError;
use basis::errors::ErrorWithSource;

use ast::Toplevel;
use token::Tokenizer;

use error::Error;
pub struct NodeEnv {
    next_id_: u32,
}

impl NodeEnv {
    pub fn new() -> Self {
        NodeEnv { next_id_: 0 }
    }

    fn next_id(&mut self) -> ast::NodeId {
        let n = self.next_id_;
        self.next_id_ += 1;
        ast::NodeId::new(n)
    }

    pub fn parse(&mut self, input: &str) -> Result<Vec<Spanned<Toplevel>>, BasisError<Error>> {
        let tokens = Tokenizer::new(input).collect::<Result<Vec<_>, _>>().map_err(|err| BasisError::new(err.map(Error::from)))?;

        grammar::parse_Program(input, self, tokens.into_iter().map(|sp| {
            let span = sp.span;
            (span.start, sp.value, span.end)
        })).map_err(|lalrpop_error| BasisError::new(Error::from_lalrpop(lalrpop_error)))
    }
}

type MutNodeEnv<'a> = &'a mut NodeEnv;

pub fn parse(source: &Source) -> Result<Vec<Spanned<Toplevel>>, ErrorWithSource<Error>> {
    let mut env = NodeEnv::new();
    env.parse(&source.contents).map_err(|err| ErrorWithSource::new(err, source))
}

#[test]
fn test_parse() {
    let input = r#"
    let x: int = 1;
    let y: int = 2;
    def add(x: int, y: int): int = {
        let z = x + y;
        z
    }
        "#;
    NodeEnv::new().parse(input).unwrap();
}
