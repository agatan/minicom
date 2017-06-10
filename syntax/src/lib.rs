#![feature(box_syntax)]

#[macro_use]
extern crate quick_error;
extern crate lalrpop_util;

pub mod ast;
mod token;
mod source;
pub mod pos;
mod grammar;

pub use source::Source;

use std::convert::From;

use ast::Toplevel;
use pos::{DUMMY_LOCATION, Location, Spanned};
use token::{Token, Tokenizer, Error as TokenizeError};

quick_error! {
    #[derive(Debug, PartialEq)]
    pub enum Error {
        Token(err: TokenizeError) {
            description(err.description())
            display("{}", err)
            from()
        }
        InvalidToken {
            description("invalid token")
            display("invalid token")
        }
        UnexpectedToken(token: String, expected: Vec<String>) {
            description("unexpected token")
            display("unexpected token: {} (expected {:?})", token, expected)
        }
        UnexpectedEof(expected: Vec<String>) {
            description("unexpected EOF")
            display("unexpected EOF (expected {:?})", expected)
        }
        ExtraToken(token: String) {
            description("extra token")
            display("extra token: {}", token)
        }
    }
}

impl Error {
    fn from_lalrpop<'input>(err: lalrpop_util::ParseError<Location, Token<'input>, ()>) -> Spanned<Error> {
        use lalrpop_util::ParseError::*;
        match err {
            InvalidToken { location } => Spanned::new(location, location, Error::InvalidToken),
            UnrecognizedToken { token: Some((start, token, end)), expected } =>
                Spanned::new(start, end, Error::UnexpectedToken(token.to_string(), expected)),
            UnrecognizedToken { token: None, expected } =>
                Spanned::new(DUMMY_LOCATION, DUMMY_LOCATION, Error::UnexpectedEof(expected)),
            ExtraToken { token: (start, token, end) } => Spanned::new(start, end , Error::ExtraToken(token.to_string())),
            User { error: () } => unreachable!(),
        }
    }
}

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

    pub fn parse(&mut self, input: &str) -> Result<Vec<Spanned<Toplevel>>, Spanned<Error>> {
        let tokens = Tokenizer::new(input).collect::<Result<Vec<_>, _>>().map_err(|err| err.map(Error::from))?;

        grammar::parse_Program(input, self, tokens.into_iter().map(|sp| {
            let span = sp.span;
            (span.start, sp.value, span.end)
        })).map_err(Error::from_lalrpop)
    }
}

type MutNodeEnv<'a> = &'a mut NodeEnv;

pub fn parse(source: &Source) -> Result<Vec<Spanned<Toplevel>>, Spanned<Error>> {
    let mut env = NodeEnv::new();
    env.parse(&source.contents)
}

#[test]
fn test_parse() {
    use token::Tokenizer;
    let mut env = NodeEnv::new();
    let input = r#"
    let x: int = 1;
    let y: int = 2;
    def add(x: int, y: int): int {
        let z = x + y;
        z
    }
        "#;
    let tokens = Tokenizer::new(input).collect::<Result<Vec<_>, _>>().unwrap();
    grammar::parse_Program(input, &mut env, tokens.into_iter().map(|sp| {
        let span = sp.span;
        (span.start, sp.value, span.end)
    })).unwrap();
}
