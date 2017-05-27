use std::str::Chars;
use std::iter::Iterator;
use std::fmt;

use pos::{Byte, Line, Column, Location, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    Identifier(&'input str),
    IntLiteral(i64),
    FloatLiteral(f64),

    Add,
    Sub,
    Mul,
    Div,
    Equals,

    Let,
    Def,
    Print,

    Colon,
    Comma,
    LParen,
    RParen,
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
            Identifier(_) | IntLiteral(_) | FloatLiteral(_) | RParen | RBrace => true,
            _ => false,
        }
    }
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;
        match *self {
            Identifier(n) => fmt::Debug::fmt(n, f),
            IntLiteral(n) => n.fmt(f),
            FloatLiteral(n) => n.fmt(f),
            Add => "+".fmt(f),
            Sub => "-".fmt(f),
            Mul => "*".fmt(f),
            Div => "/".fmt(f),
            Equals => "=".fmt(f),
            Let => "let".fmt(f),
            Def => "def".fmt(f),
            Print => "print".fmt(f),
            Colon => ":".fmt(f),
            Comma => ",".fmt(f),
            LParen => "(".fmt(f),
            RParen => ")".fmt(f),
            LBrace => "{".fmt(f),
            RBrace => "}".fmt(f),
            Semi => ";".fmt(f),
            ImplicitSemi => "<newline>".fmt(f),
            EOF => "<EOF>".fmt(f),
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
    location: Location,
    chars: Chars<'input>,
}

impl<'input> CharLocations<'input> {
    fn new(input: &'input str) -> Self {
        CharLocations {
            location: Location {
                line: Line(0),
                column: Column(1),
                absolute: Byte(0),
            },
            chars: input.chars(),
        }
    }
}

impl<'input> Iterator for CharLocations<'input> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().map(|ch| {
            let loc = self.location;
            self.location = self.location.shift(ch);
            (loc, ch)
        })
    }
}

pub struct Tokenizer<'input> {
    input: &'input str,
    chars: CharLocations<'input>,
    eof_location: Location,
    lookahead: Option<(Location, char)>,
    last: Option<Token<'input>>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = CharLocations::new(input);
        let eof_location = chars.location;
        Tokenizer {
            input: input,
            lookahead: chars.next(),
            eof_location: eof_location,
            chars: chars,
            last: None,
        }
    }

    fn bump(&mut self) -> Option<(Location, char)> {
        match self.lookahead {
            Some((location, ch)) => {
                self.eof_location = self.eof_location.shift(ch);
                self.lookahead = self.chars.next();
                Some((location, ch))
            }
            None => None,
        }
    }

    fn discard_all(&mut self) {
        while self.lookahead.is_some() {
            self.bump();
        }
    }

    fn error<T>(&mut self, location: Location, err: Error) -> Result<T, SpannedError> {
        self.discard_all();
        Err(Spanned::new(location, location, err))
    }

    fn substr(&self, start: Location, end: Location) -> &'input str {
        &self.input[start.absolute.0..end.absolute.0]
    }

    fn take_while<F>(&mut self, start: Location, mut f: F) -> (Location, &'input str)
        where F: FnMut(char) -> bool
    {
        while let Some((end, ch)) = self.lookahead {
            if f(ch) {
                self.bump();
            } else {
                return (end, self.substr(start, end));
            }
        }
        (self.eof_location, self.substr(start, self.eof_location))
    }

    fn identifier(&mut self, start: Location) -> SpannedToken<'input> {
        let (end, ident) = self.take_while(start, is_ident_continue);
        let token = match ident {
            "let" => Token::Let,
            "def" => Token::Def,
            "print" => Token::Print,
            ident => Token::Identifier(ident),
        };
        Spanned::new(start, end, token)
    }

    fn numeric_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, SpannedError> {
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
                '{' => Some(Ok(Spanned::new(start, start.shift(ch), Token::LBrace))),
                '}' => Some(Ok(Spanned::new(start, start.shift(ch), Token::RBrace))),
                '+' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Add))),
                '-' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Sub))),
                '*' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Mul))),
                '/' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Div))),
                '=' => Some(Ok(Spanned::new(start, start.shift(ch), Token::Equals))),
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
        None
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
    use pos::*;

    use super::*;
    use token::Token;

    fn make_loc(bytepos: usize) -> Location {
        Location {
            line: Line(0),
            column: Column(bytepos + 1),
            absolute: Byte(bytepos),
        }
    }

    fn runtest(input: &str, expected: Vec<(&str, Token)>) {
        let tokenizer = Tokenizer::new(input);

        for (token, (expected_span, expected_tok)) in tokenizer.zip(expected) {
            let start = make_loc(expected_span.find("^").unwrap());
            let end = make_loc(expected_span.rfind("^").unwrap() + 1);
            assert_eq!(Ok(Spanned::new(start, end, expected_tok)), token);
        }
    }

    #[test]
    fn identifier() {
        runtest("abc _ _x a1_",
                vec![("^^^         ", Token::Identifier("abc")),
                     ("    ^       ", Token::Identifier("_")),
                     ("      ^^    ", Token::Identifier("_x")),
                     ("         ^^^", Token::Identifier("a1_"))])
    }

    #[test]
    fn numeric() {
        runtest("123 123.4",
                vec![("^^^      ", Token::IntLiteral(123)),
                     ("    ^^^^^", Token::FloatLiteral(123.4))])
    }
}
