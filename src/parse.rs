use std::cell::Cell;

use combine::{State, Parser, ParseResult, Stream, unexpected, env_parser, eof};
use combine::primitives::ParseError;
use combine::char::{spaces, alpha_num, letter, string, char};
use combine::combinator::{EnvParser, try, sep_end_by};
use combine_language::{LanguageEnv, LanguageDef, Identifier, Assoc, Fixity, expression_parser};

use ast::{NodeId, Node, NodeKind, Let, Type, TypeKind};

type LanguageParser<'input: 'parser, 'parser, I, T> = EnvParser<&'parser ParserEnv<'input, I>,
                                                                I,
                                                                T>;

struct ParserEnv<'input, I> {
    next_node_id: Cell<u32>,
    env: LanguageEnv<'input, I>,
}

impl<'input, I> ParserEnv<'input, I>
    where I: Stream<Item = char> + 'input
{
    fn new() -> Self {
        ParserEnv {
            next_node_id: Cell::new(0),
            env: LanguageEnv::new(LanguageDef {
                ident: Identifier {
                    start: letter(),
                    rest: alpha_num(),
                    reserved: ["print", "let"].iter().map(|x| (*x).into()).collect(),
                },
                op: Identifier {
                    start: unexpected("cannot use user defined operator").map(|_| ' '),
                    rest: unexpected("cannot use user defined operator").map(|_| ' '),
                    reserved: ["+", "-", "*", "/", "="].iter().map(|x| (*x).into()).collect(),
                },
                comment_start: string("/*").map(|_| ()),
                comment_end: string("*/").map(|_| ()),
                comment_line: string("//").map(|_| ()),
            }),
        }
    }

    fn new_node_id(&self) -> NodeId {
        let x = self.next_node_id.get();
        self.next_node_id.set(x + 1);
        NodeId::new(x)
    }

    fn parse_toplevel_node(&self, input: I) -> ParseResult<Node, I> {
        choice!(try(self.let_stmt()).map(Node::from),
                self.expression().map(Node::from))
            .parse_stream(input)
    }

    pub fn toplevel_node<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_toplevel_node)
    }

    fn parse_toplevel_code(&self, input: I) -> ParseResult<Vec<Node>, I> {
        sep_end_by(self.toplevel_node(), self.env.lex(char(';'))).parse_stream(input)
    }

    pub fn toplevel_code<'p>(&'p self) -> LanguageParser<'input, 'p, I, Vec<Node>> {
        env_parser(self, ParserEnv::parse_toplevel_code)
    }

    // statements

    fn parse_let_stmt(&self, input: I) -> ParseResult<Node, I> {
        (self.env.reserved("let"),
         self.env.identifier(),
         self.env.reserved_op("="),
         self.expression())
            .map(|(_, name, _, value)| {
                let let_ = Let {
                    name: name,
                    typ: None,
                    value: value,
                };
                Node {
                    id: self.new_node_id(),
                    kind: NodeKind::Let(Box::new(let_)),
                }
            })
            .parse_stream(input)
    }

    fn let_stmt<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_let_stmt)
    }

    // expressions

    fn parse_integer(&self, input: I) -> ParseResult<Node, I> {
        self.env
            .lex(self.env.integer_())
            .map(|n| Node::new(self.new_node_id(), NodeKind::Int(n)))
            .parse_stream(input)
    }

    fn integer<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_integer)
    }

    fn parse_float(&self, input: I) -> ParseResult<Node, I> {
        self.env
            .lex(self.env.float_())
            .map(|n| Node::new(self.new_node_id(), NodeKind::Float(n)))
            .parse_stream(input)
    }

    fn float<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_float)
    }

    fn parse_parens_expr(&self, input: I) -> ParseResult<Node, I> {
        self.env
            .lex(self.env.parens(self.expression()))
            .map(|e| Node::new(self.new_node_id(), NodeKind::Parens(Box::new(e))))
            .parse_stream(input)
    }

    fn parens_expr<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_parens_expr)
    }

    fn parse_print_expr(&self, input: I) -> ParseResult<Node, I> {
        (self.env.reserved("print"),
         self.env.lex(char('(')),
         self.expression(),
         self.env.lex(char(')')))
            .map(|(_, _, e, _)| Node::new(self.new_node_id(), NodeKind::Print(Box::new(e))))
            .parse_stream(input)
    }

    fn print_expr<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_print_expr)
    }

    fn parse_term_expr(&self, input: I) -> ParseResult<Node, I> {
        choice!(self.parens_expr(),
                self.print_expr(),
                try(self.float()),
                self.integer())
            .parse_stream(input)
    }

    fn term_expr<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_term_expr)
    }

    fn parse_expression(&self, input: I) -> ParseResult<Node, I> {
        let new_binop = |l: Node, o: &str, r: Node| -> Node {
            let kind = match o {
                "+" => NodeKind::Add(box l, box r),
                "-" => NodeKind::Sub(box l, box r),
                "*" => NodeKind::Mul(box l, box r),
                "/" => NodeKind::Div(box l, box r),
                _ => unreachable!(),
            };
            Node::new(self.new_node_id(), kind)
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

    fn expression<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_expression)
    }
}

pub fn parse_expression(input: &str) -> Result<Node, ParseError<State<&str>>> {
    let env = ParserEnv::new();
    match spaces().with(env.expression()).skip(eof()).parse_stream(State::new(input)) {
        Ok((expr, _)) => Ok(expr),
        Err(err) => Err(err.into_inner()),
    }
}

pub fn parse_toplevel_node(input: &str) -> Result<Node, ParseError<State<&str>>> {
    let env = ParserEnv::new();
    match spaces().with(env.toplevel_node()).skip(eof()).parse_stream(State::new(input)) {
        Ok((node, _)) => Ok(node),
        Err(err) => Err(err.into_inner()),
    }
}

pub fn parse(input: &str) -> Result<Vec<Node>, ParseError<State<&str>>> {
    let env = ParserEnv::new();
    match spaces().with(env.toplevel_code()).skip(eof()).parse_stream(State::new(input)) {
        Ok((node, _)) => Ok(node),
        Err(err) => Err(err.into_inner()),
    }
}
