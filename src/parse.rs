use combine::{State, Parser, ParseResult, Stream, unexpected, env_parser, eof};
use combine::primitives::ParseError;
use combine::char::{spaces, alpha_num, letter, string};
use combine::combinator::EnvParser;
use combine_language::{LanguageEnv, LanguageDef, Identifier, Assoc, Fixity, expression_parser};

use ast::Expr;

type LanguageParser<'input: 'parser, 'parser, I, T> = EnvParser<&'parser ParserEnv<'input, I>,
                                                                I,
                                                                T>;

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
                    start: unexpected("cannot use user defined operator").map(|_| ' '),
                    rest: unexpected("cannot use user defined operator").map(|_| ' '),
                    reserved: vec!["+"].iter().map(|x| (*x).into()).collect(),
                },
                comment_start: string("/*").map(|_| ()),
                comment_end: string("*/").map(|_| ()),
                comment_line: string("//").map(|_| ()),
            }),
        }
    }

    fn parse_integer(&self, input: I) -> ParseResult<Expr, I> {
        self.env.lex(self.env.integer_()).map(Expr::Int).parse_stream(input)
    }

    fn integer<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_integer)
    }

    fn parse_expression(&self, input: I) -> ParseResult<Expr, I> {
        fn op<'a>(l: Expr, o: &'a str, r: Expr) -> Expr {
            match o {
                "+" => Expr::Add(box l, box r),
                _ => unreachable!(),
            }
        }
        let op_parser = self.env.reserved_op_("+").map(|op| {
            (op,
             Assoc {
                 precedence: 6,
                 fixity: Fixity::Left,
             })
        });
        expression_parser(self.integer(), op_parser, op).parse_stream(input)
    }

    fn expression<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_expression)
    }
}

pub fn parse_expression(input: &str) -> Result<Expr, ParseError<State<&str>>> {
    let env = ParserEnv::new();
    match spaces().with(env.expression()).skip(eof()).parse_stream(State::new(input)) {
        Ok((expr, _)) => Ok(expr),
        Err(err) => Err(err.into_inner()),
    }
}
