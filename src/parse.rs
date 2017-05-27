use std::cell::Cell;

use combine::{State, Parser, ParseResult, Stream, unexpected, env_parser, eof};
use combine::primitives::ParseError;
use combine::char::{spaces, alpha_num, letter, string, char};
use combine::combinator::{EnvParser, try, sep_end_by, optional};
use combine_language::{LanguageEnv, LanguageDef, Identifier, Assoc, Fixity, expression_parser};

use ast::{NodeId, Node, NodeKind, Let, Type, Def};

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
                    reserved: ["print", "let", "def"].iter().map(|x| (*x).into()).collect(),
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

    fn parse_node(&self, input: I) -> ParseResult<Node, I> {
        choice!(self.def(), self.statement(), self.expression()).parse_stream(input)
    }

    pub fn node<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_node)
    }

    fn parse_toplevel_code(&self, input: I) -> ParseResult<Vec<Node>, I> {
        sep_end_by(self.node(), self.env.lex(char(';'))).parse_stream(input)
    }

    pub fn toplevel_code<'p>(&'p self) -> LanguageParser<'input, 'p, I, Vec<Node>> {
        env_parser(self, ParserEnv::parse_toplevel_code)
    }

    // definition
    fn parse_def(&self, input: I) -> ParseResult<Node, I> {
        let arg = (self.env.identifier(), self.typespec());
        let args = sep_end_by(arg, self.env.lex(char(',')));
        (self.env.reserved("def"),
         self.env.identifier(),
         self.env.parens(args),
         optional(self.typespec()),
         self.env.braces(sep_end_by(self.node(), self.env.lex(char(';')))))
            .map(|(_def, name, args, ret, body)| {
                let def = Def {
                    name: name,
                    args: args,
                    ret: ret,
                    body: body,
                };
                Node::new(self.new_node_id(), NodeKind::Def(Box::new(def)))
            })
            .parse_stream(input)
    }

    fn def<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_def)
    }

    // statements

    fn parse_let_stmt(&self, input: I) -> ParseResult<Node, I> {
        (self.env.reserved("let"),
         self.env.identifier(),
         optional(self.typespec()),
         self.env.reserved_op("="),
         self.expression())
            .map(|(_, name, typ, _, value)| {
                let let_ = Let {
                    name: name,
                    typ: typ,
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

    fn parse_assignment(&self, input: I) -> ParseResult<Node, I> {
        (self.env.identifier(), self.env.reserved_op("="), self.expression())
            .map(|(name, _, value)| {
                Node {
                    id: self.new_node_id(),
                    kind: NodeKind::Assign(name, Box::new(value)),
                }
            })
            .parse_stream(input)
    }

    fn assignment<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_assignment)
    }

    fn parse_statement(&self, input: I) -> ParseResult<Node, I> {
        choice![self.let_stmt(), try(self.assignment())].parse_stream(input)
    }

    fn statement<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_statement)
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

    fn parse_identifier(&self, input: I) -> ParseResult<Node, I> {
        self.env
            .identifier()
            .map(|n| Node::new(self.new_node_id(), NodeKind::Ident(n)))
            .parse_stream(input)
    }

    fn identifier<'p>(&'p self) -> LanguageParser<'input, 'p, I, Node> {
        env_parser(self, ParserEnv::parse_identifier)
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
                self.identifier(),
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

    // aux functions

    fn parse_typespec(&self, input: I) -> ParseResult<Type, I> {
        self.env
            .lex(char(':'))
            .and(self.env.identifier())
            .map(|(_, name)| Type::new(name))
            .parse_stream(input)
    }

    fn typespec<'p>(&'p self) -> LanguageParser<'input, 'p, I, Type> {
        env_parser(self, ParserEnv::parse_typespec)
    }
}

pub fn parse(input: &str) -> Result<Vec<Node>, ParseError<State<&str>>> {
    let env = ParserEnv::new();
    match spaces().with(env.toplevel_code()).skip(eof()).parse_stream(State::new(input)) {
        Ok((node, _)) => Ok(node),
        Err(err) => Err(err.into_inner()),
    }
}
