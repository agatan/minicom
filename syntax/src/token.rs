use std::str::Chars;
use std::iter::Iterator;
use std::fmt;

use basis::sourcemap::{Source, Pos, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    UpperIdentifier(&'input str),
    RefType,
    Identifier(&'input str),
    IntLiteral(i32),
    FloatLiteral(f64),
    True,
    False,

    Add,
    Sub,
    Mul,
    Div,
    Equals,
    EqEq,
    Neq,
    LE,
    LT,
    GE,
    GT,

    If,
    Else,
    While,
    Let,
    Def,
    Ref,
    Deref,
    LeftArrow,

    Colon,
    Comma,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,

    Semi,
    ImplicitSemi,
    EOF,
}

impl<'input> Token<'input> {
    fn follows_implicit_semi(&self) -> bool {
        use self::Token::*;
        match *self {
            UpperIdentifier(_) |
            Identifier(_) |
            IntLiteral(_) |
            FloatLiteral(_) |
            True |
            False |
            RParen |
            RBrack |
            RBrace => true,
            _ => false,
        }
    }
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            UpperIdentifier(n) => fmt::Debug::fmt(n, f),
            RefType => f.write_str("Ref"),
            Identifier(n) => fmt::Debug::fmt(n, f),
            IntLiteral(n) => n.fmt(f),
            FloatLiteral(n) => n.fmt(f),
            True => f.write_str("true"),
            False => f.write_str("false"),
            Add => f.write_str("+"),
            Sub => f.write_str("-"),
            Mul => f.write_str("*"),
            Div => f.write_str("/"),
            Equals => f.write_str("="),
            EqEq => f.write_str("=="),
            Neq => f.write_str("!="),
            LE => f.write_str("<="),
            LT => f.write_str("<"),
            GE => f.write_str(">="),
            GT => f.write_str(">"),
            If => f.write_str("if"),
            Else => f.write_str("else"),
            While => f.write_str("while"),
            Let => f.write_str("let"),
            Def => f.write_str("def"),
            Ref => f.write_str("ref"),
            Deref => f.write_str("@"),
            LeftArrow => f.write_str("<-"),
            Colon => f.write_str(":"),
            Comma => f.write_str(","),
            LParen => f.write_str("("),
            RParen => f.write_str(")"),
            LBrack => f.write_str("["),
            RBrack => f.write_str("]"),
            LBrace => f.write_str("{"),
            RBrace => f.write_str("}"),
            Semi => f.write_str(";"),
            ImplicitSemi => f.write_str("<newline>"),
            EOF => f.write_str("<EOF>"),
        }
    }
}

type SpannedToken<'input> = Spanned<Token<'input>>;

quick_error! {
    #[derive(Debug, PartialEq)]
    pub enum Error {
        UnexpectedChar(ch: char) {
            description("unexpected character")
        }
    }
}

pub type SpannedError = Spanned<Error>;

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
    is_ident_start(ch) || ch.is_digit(10)
}

struct CharLocations<'input> {
    pos: Pos,
    chars: Chars<'input>,
}

impl<'input> CharLocations<'input> {
    fn new(base: Pos, input: &'input str) -> Self {
        CharLocations {
            pos: base,
            chars: input.chars(),
        }
    }
}

impl<'input> Iterator for CharLocations<'input> {
    type Item = (Pos, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars
            .next()
            .map(|ch| {
                     let pos = self.pos;
                     self.pos = self.pos.shift(ch);
                     (pos, ch)
                 })
    }
}

pub struct Tokenizer<'input> {
    src: &'input Source,
    chars: CharLocations<'input>,
    eof_pos: Pos,
    lookahead: Option<(Pos, char)>,
    last: Option<Token<'input>>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(src: &'input Source) -> Self {
        let mut chars = CharLocations::new(src.base, &src.contents);
        let eof_pos = chars.pos;
        Tokenizer {
            src: src,
            lookahead: chars.next(),
            eof_pos: eof_pos,
            chars: chars,
            last: None,
        }
    }

    fn bump(&mut self) -> Option<(Pos, char)> {
        match self.lookahead {
            Some((pos, ch)) => {
                self.eof_pos = self.eof_pos.shift(ch);
                self.lookahead = self.chars.next();
                Some((pos, ch))
            }
            None => None,
        }
    }

    fn discard_all(&mut self) {
        while self.lookahead.is_some() {
            self.bump();
        }
    }

    fn error<T>(&mut self, pos: Pos, err: Error) -> Result<T, SpannedError> {
        self.discard_all();
        Err(Spanned::new(pos, pos, err))
    }

    fn substr(&self, start: Pos, end: Pos) -> &'input str {
        self.src.substr(start, end)
    }

    fn test_lookahead<F>(&self, mut f: F) -> bool
        where F: FnMut(char) -> bool
    {
        self.lookahead
            .as_ref()
            .map(|&(_, ch)| f(ch))
            .unwrap_or(false)
    }

    fn take_while<F>(&mut self, start: Pos, mut f: F) -> (Pos, &'input str)
        where F: FnMut(char) -> bool
    {
        while let Some((end, ch)) = self.lookahead {
            if f(ch) {
                self.bump();
            } else {
                return (end, self.substr(start, end));
            }
        }
        (self.eof_pos, self.substr(start, self.eof_pos))
    }

    fn identifier(&mut self, start: Pos) -> SpannedToken<'input> {
        let (end, ident) = self.take_while(start, is_ident_continue);
        let token = match ident {
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "let" => Token::Let,
            "def" => Token::Def,
            "ref" => Token::Ref,
            "Ref" => Token::RefType,
            ident => {
                if ident.starts_with(|ch: char| ch.is_uppercase()) {
                    Token::UpperIdentifier(ident)
                } else {
                    Token::Identifier(ident)
                }
            }
        };
        Spanned::new(start, end, token)
    }

    fn numeric_literal(&mut self, start: Pos) -> Result<SpannedToken<'input>, SpannedError> {
        let (end, int) = self.take_while(start, |ch: char| ch.is_digit(10));

        let (start, end, token) = match self.lookahead {
            Some((_, '.')) => {
                self.bump();
                let (end, float) = self.take_while(start, |ch: char| ch.is_digit(10));
                (start, end, Token::FloatLiteral(float.parse().unwrap()))
            }
            Some((start, ch)) if is_ident_start(ch) => {
                return self.error(start, Error::UnexpectedChar(ch))
            }
            None | Some(_) => (start, end, Token::IntLiteral(int.parse().unwrap())),
        };

        Ok(Spanned::new(start, end, token))
    }

    fn next_token(&mut self) -> Option<Result<SpannedToken<'input>, SpannedError>> {
        while let Some((start, ch)) = self.bump() {
            return match ch {
                       ',' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Comma))),
                       ':' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Colon))),
                       ';' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Semi))),
                       '(' => Some(Ok(Spanned::new(start, start.shift(ch), Token::LParen))),
                       ')' => Some(Ok(Spanned::new(start, start.shift(ch), Token::RParen))),
                       '[' => Some(Ok(Spanned::new(start, start.shift(ch), Token::LBrack))),
                       ']' => Some(Ok(Spanned::new(start, start.shift(ch), Token::RBrack))),
                       '{' => Some(Ok(Spanned::new(start, start.shift(ch), Token::LBrace))),
                       '}' => Some(Ok(Spanned::new(start, start.shift(ch), Token::RBrace))),
                       '@' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Deref))),
                       '+' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Add))),
                       '-' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Sub))),
                       '*' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Mul))),
                       '/' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Div))),
                       '=' => {
                           let sp = if self.test_lookahead(|ch| ch == '=') {
                               self.bump();
                               Spanned::new(start, start.shift(ch).shift('='), Token::EqEq)
                           } else {
                               Spanned::new(start, start.shift(ch), Token::Equals)
                           };
                           Some(Ok(sp))
                       }
                       '!' => {
                           let sp = if self.test_lookahead(|ch| ch == '=') {
                               self.bump();
                               Spanned::new(start, start.shift(ch).shift('='), Token::Neq)
                           } else {
                               return Some(self.error(start, Error::UnexpectedChar(ch)));
                           };
                           Some(Ok(sp))
                       }
                       '<' => {
                           let sp = if self.test_lookahead(|ch| ch == '=') {
                               self.bump();
                               Spanned::new(start, start.shift(ch).shift('='), Token::LE)
                           } else if self.test_lookahead(|ch| ch == '-') {
                               self.bump();
                               Spanned::new(start, start.shift(ch).shift('-'), Token::LeftArrow)
                           } else {
                               Spanned::new(start, start.shift(ch), Token::LT)
                           };
                           Some(Ok(sp))
                       }
                       '>' => {
                           let sp = if self.test_lookahead(|ch| ch == '=') {
                               self.bump();
                               Spanned::new(start, start.shift(ch).shift('='), Token::GE)
                           } else {
                               Spanned::new(start, start.shift(ch), Token::GT)
                           };
                           Some(Ok(sp))
                       }
                       ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),
                       ch if ch.is_digit(10) => Some(self.numeric_literal(start)),
                       '\n' => {
                           match self.last {
                               Some(ref tok) if tok.follows_implicit_semi() => {
                                   Some(Ok(Spanned::new(start, start, Token::ImplicitSemi)))
                               }
                               _ => continue,
                           }
                       }
                       ch if ch.is_whitespace() => continue,
                       ch => Some(self.error(start, Error::UnexpectedChar(ch))),
                   };
        }
        match self.last {
            Some(ref tok) if tok.follows_implicit_semi() => {
                Some(Ok(Spanned::new(self.eof_pos, self.eof_pos, Token::ImplicitSemi)))
            }
            _ => None,
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<SpannedToken<'input>, SpannedError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Some(Ok(token)) => {
                self.last = Some(token.value.clone());
                Some(Ok(token))
            }
            other => other,
        }
    }
}

#[cfg(test)]
mod test {
    use basis::sourcemap::*;

    use super::*;
    use token::Token;

    fn make_pos(bytepos: usize) -> Pos {
        Pos(1 + bytepos)
    }

    fn runtest(input: &str, expected: Vec<(&str, Token)>) {
        let mut srcmap = SourceMap::new();
        let src = srcmap.add_dummy(input.to_string());
        let tokenizer = Tokenizer::new(&*src);

        for (token, (expected_span, expected_tok)) in tokenizer.zip(expected) {
            let start = make_pos(expected_span.find("^").unwrap());
            let end = make_pos(expected_span.rfind("^").unwrap() + 1);
            assert_eq!(Ok(Spanned::new(start, end, expected_tok)), token);
        }
    }

    #[test]
    fn identifier() {
        runtest("abc _ _x a1_ Int ",
                vec![("^^^              ", Token::Identifier("abc")),
                     ("    ^            ", Token::Identifier("_")),
                     ("      ^^         ", Token::Identifier("_x")),
                     ("         ^^^     ", Token::Identifier("a1_")),
                     ("             ^^^ ", Token::UpperIdentifier("Int"))])
    }

    #[test]
    fn numeric() {
        runtest("123 123.4",
                vec![("^^^      ", Token::IntLiteral(123)),
                     ("    ^^^^^", Token::FloatLiteral(123.4))])
    }

    #[test]
    fn multiple_char_operators() {
        runtest(" == != <= < >= > @ <- ",
                vec![(" ^^                   ", Token::EqEq),
                     ("    ^^                ", Token::Neq),
                     ("       ^^             ", Token::LE),
                     ("          ^           ", Token::LT),
                     ("            ^^        ", Token::GE),
                     ("               ^      ", Token::GT),
                     ("                 ^    ", Token::Deref),
                     ("                   ^^ ", Token::LeftArrow)])
    }

    #[test]
    fn implicit_semicolons() {
        let tests = ["1\n", "1.2", "true\n", "false", "foo\n", ")", "}\n"];
        for test in tests.iter() {
            let mut srcmap = SourceMap::new();
            let src = srcmap.add_dummy(test.to_string());
            let mut tokenizer = Tokenizer::new(&*src);
            assert!(tokenizer.next().unwrap().is_ok());
            assert_eq!(tokenizer.next().unwrap().unwrap().value,
                       Token::ImplicitSemi);
        }
    }
}
