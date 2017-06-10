use std::fmt;
use std::io::prelude::*;
use std::io;
use std::fs::File;

#[derive(Debug, Clone)]
pub struct Source {
    pub path: String,
    pub contents: String,
}

impl Source {
    pub fn from_file(filename: String) -> io::Result<Self> {
        let mut file = File::open(&filename)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(Source {
               path: filename,
               contents: contents,
           })
    }

    pub fn from_stdin() -> io::Result<Self> {
        let mut contents = String::new();
        io::stdin().read_to_string(&mut contents)?;
        Ok(Source {
               path: "<stdin>".to_string(),
               contents: contents,
           })
    }

    pub fn with_dummy(dummy: String) -> Self {
        Source {
            path: "<dummy>".to_string(),
            contents: dummy,
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Byte(pub usize);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Line(pub usize);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Column(pub usize);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub line: Line,
    pub column: Column,
    pub absolute: Byte,
}

pub const DUMMY_LOCATION: Location = Location {
    line: Line(!0),
    column: Column(!0),
    absolute: Byte(!0),
};

impl Location {
    pub fn shift(mut self, ch: char) -> Location {
        if ch == '\n' {
            self.line.0 += 1;
            self.column.0 = 1;
        } else {
            self.column.0 += 1;
        }
        self.absolute.0 += ch.len_utf8();
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Span {
            start: start,
            end: end,
        }
    }

    pub fn getline<'s>(&self, input: &'s str) -> Option<&'s str> {
        let start = self.start.absolute.0;
        if start >= input.len() {
            return None;
        }
        let line_start = input[..start].rfind('\n').map(|x| x + 1).unwrap_or(0);
        let slice = &input[line_start..];
        let line_end = slice.find('\n').unwrap_or(slice.len());
        Some(&slice[..line_end])
    }
}

#[test]
fn test_getline() {
    let input = "abc\ndef\n";
    let tests = vec![(Location {
                          line: Line(1),
                          column: Column(1),
                          absolute: Byte(4),
                      },
                      Some("def")),
                     (Location {
                          line: Line(1),
                          column: Column(3),
                          absolute: Byte(6),
                      },
                      Some("def")),
                     (Location {
                          line: Line(1),
                          column: Column(4),
                          absolute: Byte(7),
                      },
                      Some("def")),
                     (Location {
                          line: Line(0),
                          column: Column(4),
                          absolute: Byte(3),
                      },
                      Some("abc")),
                     (Location {
                          line: Line(0),
                          column: Column(3),
                          absolute: Byte(2),
                      },
                      Some("abc")),
                     (Location {
                          line: Line(0),
                          column: Column(1),
                          absolute: Byte(0),
                      },
                      Some("abc"))];
    for (loc, expected) in tests {
        let span = Span::new(loc, loc);
        assert_eq!(span.getline(input), expected);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn new(start: Location, end: Location, value: T) -> Self {
        Spanned {
            span: Span::new(start, end),
            value: value,
        }
    }

    pub fn span(span: Span, value: T) -> Self {
        Spanned {
            span: span,
            value: value,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        let Spanned { span, value } = self;
        Spanned {
            span: span,
            value: f(value),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{}:{}: {}",
               self.span.start.line.0 + 1,
               self.span.start.column.0,
               self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn source_from_file() {
        let result = Source::from_file("./src/pos.rs".to_string()).unwrap();
        assert_eq!(result.path, "./src/pos.rs");
        assert!(!result.contents.is_empty());
    }
}
