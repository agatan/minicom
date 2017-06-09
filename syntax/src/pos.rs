use std::fmt;

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
               self.span.start.line.0,
               self.span.start.column.0,
               self.value)
    }
}
