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

use basis::sourcemap::{Source, SourceMap};
use basis::errors::Error as BasisError;
use basis::errors::ErrorWithSource;

use ast::Toplevel;
use token::Tokenizer;

use error::Error;

pub fn parse<'m>(srcmap: &'m SourceMap, source: &Source) -> Result<Vec<Toplevel>, ErrorWithSource<'m, Error>> {
    let tokens = Tokenizer::new(source)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| ErrorWithSource::new(BasisError::new(err.map(Error::from)), srcmap))?;

    grammar::parse_Program(source, tokens.into_iter().map(|sp| {
        let span = sp.span;
        (span.start, sp.value, span.end)
    })).map_err(|lalrpop_error|
                ErrorWithSource::new(BasisError::new(Error::from_lalrpop(lalrpop_error)), srcmap))
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
    parse(&srcmap, &*src).unwrap();
}

