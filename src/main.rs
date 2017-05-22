extern crate combine;
extern crate combine_language;

use std::io::Write;

use combine::{State, Parser, ParseResult, Stream, satisfy, env_parser};
use combine::char::{alpha_num, letter, string};
use combine::combinator::EnvParser;
use combine_language::{LanguageEnv, LanguageDef, Identifier};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Int(i64),
}

type LanguageParser<'input: 'parser, 'parser, I, T> = EnvParser<&'parser ParserEnv<'input, I>, I, T>;

struct ParserEnv<'input, I> {
    env: LanguageEnv<'input, I>,
}

impl<'input, I> ParserEnv<'input, I>
    where I: Stream<Item = char> + 'input
{
    fn new() -> Self {
        ParserEnv {
            env: LanguageEnv::new(LanguageDef {
                ident: Identifier {
                    start: letter(),
                    rest: alpha_num(),
                    reserved: vec![],
                },
                op: Identifier {
                    start: satisfy(|c| "+-*/".chars().any(|x| x == c)),
                    rest: satisfy(|c| "+-*/".chars().any(|x| x == c)),
                    reserved: vec![],
                },
                comment_start: string("/*").map(|_| ()),
                comment_end: string("*/").map(|_| ()),
                comment_line: string("//").map(|_| ()),
            }),
        }
    }

    fn parse_integer(&self, input: I) -> ParseResult<Expr, I> {
        self.env.integer_().map(Expr::Int).
            parse_stream(input)
    }

    fn integer<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_integer)
    }
}

fn main() {
    let input = match ::std::env::args().nth(1) {
        None => {
            writeln!(&mut std::io::stderr(), "no input given").unwrap();
            ::std::process::exit(1);
        },
        Some(input) => input,
    };
    let env = ParserEnv::new();
    let expr = match env.integer().parse(State::new(input.as_str())) {
        Ok((expr, _)) => expr,
        Err(err) => {
            write!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };

    println!("expression: {:?}", expr);
}
