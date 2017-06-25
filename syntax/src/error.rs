use lalrpop_util;

use basis::sourcemap::{NPOS, Pos, Spanned};

use token::{Token, Error as TokenizeError};

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
    pub fn from_lalrpop<'input>(err: lalrpop_util::ParseError<Pos, Token<'input>, ()>)
                                -> Spanned<Error> {
        use lalrpop_util::ParseError::*;
        match err {
            InvalidToken { location } => Spanned::new(location, location, Error::InvalidToken),
            UnrecognizedToken {
                token: Some((start, token, end)),
                expected,
            } => {
                Spanned::new(start,
                             end,
                             Error::UnexpectedToken(token.to_string(), expected))
            }
            UnrecognizedToken {
                token: None,
                expected,
            } => Spanned::new(NPOS, NPOS, Error::UnexpectedEof(expected)),
            ExtraToken { token: (start, token, end) } => {
                Spanned::new(start, end, Error::ExtraToken(token.to_string()))
            }
            User { error: () } => unreachable!(),
        }
    }
}
