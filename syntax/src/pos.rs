#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Byte(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Line(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Column(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
}
