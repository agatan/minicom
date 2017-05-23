use std::cell::Cell;

use combine::{State, Parser, ParseResult, Stream, unexpected, env_parser, eof};
use combine::primitives::ParseError;
use combine::char::{spaces, alpha_num, letter, string};
use combine::combinator::{EnvParser, try};
use combine_language::{LanguageEnv, LanguageDef, Identifier, Assoc, Fixity, expression_parser};

use ast::{Expr, ExprKind, ExprId, Type};

type LanguageParser<'input: 'parser, 'parser, I, T> = EnvParser<&'parser ParserEnv<'input, I>,
                                                                I,
                                                                T>;

struct ParserEnv<'input, I> {
    next_expr_id: Cell<u32>,
    env: LanguageEnv<'input, I>,
}

impl<'input, I> ParserEnv<'input, I>
    where I: Stream<Item = char> + 'input
{
    fn new() -> Self {
        ParserEnv {
            next_expr_id: Cell::new(0),
            env: LanguageEnv::new(LanguageDef {
                ident: Identifier {
                    start: letter(),
                    rest: alpha_num(),
                    reserved: vec![],
                },
                op: Identifier {
                    start: unexpected("cannot use user defined operator").map(|_| ' '),
                    rest: unexpected("cannot use user defined operator").map(|_| ' '),
                    reserved: ["+", "-", "*", "/"].iter().map(|x| (*x).into()).collect(),
                },
                comment_start: string("/*").map(|_| ()),
                comment_end: string("*/").map(|_| ()),
                comment_line: string("//").map(|_| ()),
            }),
        }
    }

    fn new_expr_id(&self) -> ExprId {
        let x = self.next_expr_id.get();
        self.next_expr_id.set(x + 1);
        ExprId::new(x)
    }

    fn parse_integer(&self, input: I) -> ParseResult<Expr, I> {
        self.env
            .lex(self.env.integer_())
            .map(|n| Expr::with_typ(self.new_expr_id(), ExprKind::Int(n), Type::int()))
            .parse_stream(input)
    }

    fn integer<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_integer)
    }

    fn parse_float(&self, input: I) -> ParseResult<Expr, I> {
        self.env
            .lex(self.env.float_())
            .map(|n| Expr::new(self.new_expr_id(), ExprKind::Float(n)))
            .parse_stream(input)
    }

    fn float<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_float)
    }

    fn parse_parens_expr(&self, input: I) -> ParseResult<Expr, I> {
        self.env
            .lex(self.env.parens(self.expression()))
            .map(|e| {
                let typ = e.typ.clone();
                Expr::with_typ(self.new_expr_id(), ExprKind::Parens(Box::new(e)), typ)
            })
            .parse_stream(input)
    }

    fn parens_expr<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_parens_expr)
    }

    fn parse_term_expr(&self, input: I) -> ParseResult<Expr, I> {
        choice!(self.parens_expr(), try(self.float()), self.integer()).parse_stream(input)
    }

    fn term_expr<'p>(&'p self) -> LanguageParser<'input, 'p, I, Expr> {
        env_parser(self, ParserEnv::parse_term_expr)
    }

    fn parse_expression(&self, input: I) -> ParseResult<Expr, I> {
        let new_binop = |l: Expr, o: &str, r: Expr| -> Expr {
            let kind = match o {
                "+" => ExprKind::Add(box l, box r),
                "-" => ExprKind::Sub(box l, box r),
                "*" => ExprKind::Mul(box l, box r),
                "/" => ExprKind::Div(box l, box r),
                _ => unreachable!(),
            };
            Expr::new(self.new_expr_id(), kind)
        };

        let addsub_parser = choice!(self.env.reserved_op_("+"), self.env.reserved_op_("-"))
            .map(|op| {
                (op,
                 Assoc {
                     precedence: 6,
                     fixity: Fixity::Left,
                 })
            });
        let muldiv_parser = choice!(self.env.reserved_op_("*"), self.env.reserved_op_("/"))
            .map(|op| {
                (op,
                 Assoc {
                     precedence: 7,
                     fixity: Fixity::Left,
                 })
            });
        let op_parser = choice!(addsub_parser, muldiv_parser);
        expression_parser(self.term_expr(), op_parser, new_binop).parse_stream(input)
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
