use std::fmt;
use std::io::prelude::*;
use std::io;
use std::fs::File;

use ansi_term::{ANSIString, ANSIStrings, Style};

#[derive(Debug, Clone)]
pub struct Source {
    pub is_file: bool,
    pub path: String,
    pub contents: String,
}

impl Source {
    pub fn from_file(filename: String) -> io::Result<Self> {
        let mut file = File::open(&filename)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(Source {
               is_file: true,
               path: filename,
               contents: contents,
           })
    }

    pub fn from_stdin() -> io::Result<Self> {
        let mut contents = String::new();
        io::stdin().read_to_string(&mut contents)?;
        Ok(Source {
               is_file: false,
               path: "<stdin>".to_string(),
               contents: contents,
           })
    }

    pub fn with_dummy(dummy: String) -> Self {
        Source {
            is_file: false,
            path: "<dummy>".to_string(),
            contents: dummy,
        }
    }

    pub fn get_span(&self, span: Span, style: Style) -> Option<String> {
        let start = span.start.absolute.0;
        let end = span.end.absolute.0;
        if start >= self.contents.len() {
            return None;
        }
        let line_start = self.contents[..start]
            .rfind('\n')
            .map(|x| x + 1)
            .unwrap_or(0);
        let line_end = line_start +
                       &self.contents[line_start..]
                            .find('\n')
                            .unwrap_or(self.contents.len());
        if end <= line_end {
            let head = ANSIString::from(&self.contents[line_start..start]);
            let range = style.paint(&self.contents[start..end]);
            let tail = ANSIString::from(&self.contents[end..line_end]);
            Some(format!("{}", ANSIStrings(&[head, range, tail])))
        } else {
            let head = ANSIString::from(&self.contents[line_start..start]);
            let range = style.paint(&self.contents[start..line_end]);
            Some(format!("{}", ANSIStrings(&[head, range])))
        }
    }

    pub fn stem(&self) -> String {
        if self.is_file {
            let path: &::std::path::Path = self.path.as_ref();
            path.file_stem()
                .map(|s| s.to_string_lossy().into())
                .unwrap_or("a.out".to_string())
        } else {
            "a.out".to_string()
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

pub const DUMMY_SPAN: Span = Span {
    start: DUMMY_LOCATION,
    end: DUMMY_LOCATION,
};

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

    pub fn getline<'s>(&self, source: &'s Source) -> Option<&'s str> {
        let start = self.start.absolute.0;
        let input = &source.contents;
        if start >= input.len() {
            return None;
        }
        let line_start = input[..start].rfind('\n').map(|x| x + 1).unwrap_or(0);
        let slice = &input[line_start..];
        let line_end = slice.find('\n').unwrap_or(slice.len());
        Some(&slice[..line_end])
    }
}

#[derive(Debug)]
pub struct SpanWithSource<'a> {
    span: Span,
    source: &'a Source,
}

impl<'a> SpanWithSource<'a> {
    pub fn new(span: Span, source: &'a Source) -> Self {
        SpanWithSource {
            span: span,
            source: source,
        }
    }
}

impl<'a> fmt::Display for SpanWithSource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{}:{}:{}",
               self.source.path,
               self.span.start.line.0 + 1,
               self.span.start.column.0)
    }
}

#[test]
fn test_getline() {
    let source = Source::with_dummy("abc\ndef\n".into());
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
        assert_eq!(span.getline(&source), expected);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T, S = Span> {
    pub span: S,
    pub value: T,
}

impl<T> Spanned<T, Span> {
    pub fn new(start: Location, end: Location, value: T) -> Self {
        Spanned {
            span: Span::new(start, end),
            value: value,
        }
    }
}

impl<T, S> Spanned<T, S> {
    pub fn span(span: S, value: T) -> Self {
        Spanned {
            span: span,
            value: value,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U, S> {
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

    use ansi_term::Style;

    #[test]
    fn source_from_file() {
        let result = Source::from_file("./src/pos.rs".to_string()).unwrap();
        assert_eq!(result.path, "./src/pos.rs");
        assert!(!result.contents.is_empty());
    }

    #[test]
    fn source_getline() {
        let source = Source::with_dummy("^^foo^^".into());
        let span = Span::new(Location {
                                 line: Line(0),
                                 column: Column(3),
                                 absolute: Byte(2),
                             },
                             Location {
                                 line: Line(0),
                                 column: Column(6),
                                 absolute: Byte(5),
                             });
        let style = Style::new().underline();
        assert_eq!(source.get_span(span, style.clone()).unwrap(),
                   format!("^^{}^^", style.paint("foo")));
    }
}
