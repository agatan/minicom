use combine::{State, Parser, ParseResult, Stream, satisfy, env_parser, eof};
use combine::primitives::ParseError;
use combine::char::{spaces, alpha_num, letter, string};
use combine::combinator::EnvParser;
use combine_language::{LanguageEnv, LanguageDef, Identifier};

use ast::Expr;

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
        self.env.lex(self.env.integer_()).map(Expr::Int).
            parse_stream(input)
    }

    fn integer<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_integer)
    }
}

pub fn parse_expression(input: &str) -> Result<Expr, ParseError<State<&str>>> {
    let env = ParserEnv::new();
    match spaces().with(env.integer()).skip(eof()).parse_stream(State::new(input)) {
        Ok((expr, _)) => Ok(expr),
        Err(err) =>  Err(err.into_inner()),
    }
}
