#![feature(box_syntax)]

extern crate minicom_basis as basis;

#[macro_use]
extern crate quick_error;
extern crate lalrpop_util;

pub mod ast;
mod token;
mod grammar;
mod error;

use std::convert::From;

use basis::sourcemap::{Source, SourceMap, Spanned};
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

    pub fn parse(&mut self, src: &Source) -> Result<Vec<Spanned<Toplevel>>, BasisError<Error>> {
        let tokens = Tokenizer::new(src).collect::<Result<Vec<_>, _>>().map_err(|err| BasisError::new(err.map(Error::from)))?;

        grammar::parse_Program(src, self, tokens.into_iter().map(|sp| {
            let span = sp.span;
            (span.start, sp.value, span.end)
        })).map_err(|lalrpop_error| BasisError::new(Error::from_lalrpop(lalrpop_error)))
    }
}

type MutNodeEnv<'a> = &'a mut NodeEnv;

pub fn parse<'m>(srcmap: &'m SourceMap, source: &Source) -> Result<Vec<Spanned<Toplevel>>, ErrorWithSource<'m, Error>> {
    let mut env = NodeEnv::new();
    env.parse(source).map_err(|err| ErrorWithSource::new(err, srcmap))
}

#[test]
fn test_parse() {
    let mut srcmap = SourceMap::new();
    let input = r#"
    let x: Int = 1;
    let y: Int = 2;
    def add(x: Int, y: Int): Int = {
        let z = x + y;
        let r = ref(z);
        print(x)
        r <- 1
        @r
    }

    let r: Ref[Int] = ref(0)

        "#;
    let src = srcmap.add_dummy(input.to_string());
    NodeEnv::new().parse(&*src).unwrap();
}

