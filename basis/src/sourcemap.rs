use std::io::prelude::*;
use std::io;
use std::fs;
use std::rc::Rc;
use std::fmt;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Byte(pub usize);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Line(pub usize);

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Column(pub usize);

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub filename: Rc<String>,
    pub line: Line,
    pub column: Column,
    pub absolute: Byte,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// `Pos` is a light-weight representation of source position.
/// `Pos` can be converted to canonical position with `SourceMap`.
pub struct Pos(pub usize);

impl Pos {
    pub fn shift(self, ch: char) -> Self {
        Pos(self.0 + ch.len_utf8())
    }
}

/// `NPOS` is a dummy position
pub const NPOS: Pos = Pos(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn display<'a>(&self, srcmap: &'a SourceMap) -> DisplaySpan<'a> {
        DisplaySpan {
            span: *self,
            sourcemap: srcmap,
        }
    }
}

/// `NSPAN` is a dummy span
pub const NSPAN: Span = Span {
    start: NPOS,
    end: NPOS,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T, S = Span> {
    pub span: S,
    pub value: T,
}

impl<T> Spanned<T, Span> {
    pub fn new(start: Pos, end: Pos, value: T) -> Self {
        Spanned {
            span: Span {
                start: start,
                end: end,
            },
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


#[derive(Debug, Clone, PartialEq)]
/// `Source` is a source file.
pub struct Source {
    /// `base` is a base offset of this source in `SourceMap`
    pub base: Pos,
    /// `size` is a size of its contents
    pub size: usize,
    pub name: Rc<String>,
    pub contents: String,
    lines: Vec<usize>,
}

impl Source {
    fn new(base: usize, name: String, contents: String) -> Self {
        let lines = Some(0)
            .into_iter()
            .chain(contents
                       .as_str()
                       .char_indices()
                       .filter_map(|(i, c)| if c == '\n' { Some(i + 1) } else { None }))
            .chain(Some(contents.len() + 1))
            .collect::<Vec<usize>>();
        Source {
            base: Pos(base),
            size: contents.len(),
            name: Rc::new(name),
            contents: contents,
            lines: lines,
        }
    }

    pub fn executable_name(&self) -> String {
        let path: &::std::path::Path = self.name.as_ref().as_ref();
        if let Some(ext) = path.extension() {
            if ext == "mini" {
                return path.file_stem()
                           .map(|s| s.to_string_lossy().into())
                           .unwrap_or("a.out".to_string());
            }
        }
        "a.out".to_string()
    }

    pub fn shift(&self, pos: Pos) -> Pos {
        debug_assert!(pos.0 <= self.size);
        Pos(pos.0 + self.base.0)
    }

    /// Returns line number, start index and end index
    fn line_number_and_indices(&self, pos: Pos) -> Option<(usize, usize, usize)> {
        if pos.0 < self.base.0 || self.base.0 + self.size <= pos.0 {
            return None;
        }
        let offset = pos.0 - self.base.0;
        let mut newline_start = 0usize;
        for (i, &newline) in self.lines.iter().enumerate() {
            if newline > offset {
                return Some((i, newline_start, newline));
            }
            newline_start = newline;
        }
        None
    }

    pub fn line(&self, pos: Pos) -> Option<&str> {
        self.line_number_and_indices(pos)
            .map(|(_, start, end)| &self.contents[start..end - 1])
    }

    pub fn position(&self, pos: Pos) -> Option<Position> {
        let (line, line_start, _) = match self.line_number_and_indices(pos) {
            Some(x) => x,
            None => return None,
        };
        let offset = pos.0 - self.base.0;
        let position = Position {
            filename: self.name.clone(),
            line: Line(line),
            column: Column(offset - line_start + 1),
            absolute: Byte(offset),
        };
        Some(position)
    }

    pub fn substr(&self, start: Pos, end: Pos) -> &str {
        let start = start.0 - self.base.0;
        let end = end.0 - self.base.0;
        &self.contents[start..end]
    }
}

#[derive(Debug, Clone)]
/// `SourceMap` holds source files and convert light-weight `Pos` into canonical positions.
pub struct SourceMap {
    /// base offset for the next source
    base: usize,
    sources: Vec<Rc<Source>>,
}

impl SourceMap {
    pub fn new() -> Self {
        SourceMap {
            base: 1, // 0 means invalid dummy pos
            sources: Vec::new(),
        }
    }

    pub fn add(&mut self, name: String, contents: String) -> Rc<Source> {
        let base = self.base;
        self.base += contents.len();
        let source = Source::new(base, name, contents);
        let rc_source = Rc::new(source);
        self.sources.push(rc_source.clone());
        rc_source
    }

    pub fn add_file_source(&mut self, filename: String) -> io::Result<Rc<Source>> {
        let mut file = fs::File::open(&filename)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(self.add(filename, contents))
    }

    pub fn add_dummy(&mut self, contents: String) -> Rc<Source> {
        self.add("<dummy>".to_string(), contents)
    }

    fn search_sources(&self, x: usize) -> Option<&Rc<Source>> {
        let mut found = None;
        for source in self.sources.iter() {
            if source.base.0 > x {
                return found;
            }
            found = Some(source);
        }
        found
    }

    fn source_ref(&self, pos: Pos) -> Option<&Rc<Source>> {
        if pos == NPOS {
            return None;
        }
        if let Some(source) = self.search_sources(pos.0) {
            if pos.0 < source.base.0 + source.size {
                return Some(source);
            }
        }
        None
    }

    pub fn source(&self, pos: Pos) -> Option<Rc<Source>> {
        self.source_ref(pos).cloned()
    }

    pub fn line(&self, pos: Pos) -> Option<&str> {
        self.source_ref(pos).and_then(|f| f.line(pos))
    }

    pub fn position(&self, pos: Pos) -> Option<Position> {
        self.source_ref(pos).and_then(|f| f.position(pos))
    }
}

#[derive(Debug)]
pub struct DisplaySpan<'a> {
    span: Span,
    sourcemap: &'a SourceMap,
}

impl<'a> fmt::Display for DisplaySpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let position = match self.sourcemap.position(self.span.start) {
            Some(s) => s,
            None => return Ok(()),
        };
        write!(f,
               "{}:{}:{}",
               position.filename,
               position.line.0,
               position.column.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_sourcemap() {
        let mut sourcemap = SourceMap::new();
        sourcemap.add_dummy("012".to_string());
        sourcemap.add_dummy("345".to_string());
        let f = sourcemap.source(Pos(1)).expect("Pos(1)");
        assert_eq!(f.contents, "012", "Pos(1)");
        let f = sourcemap.source(Pos(3)).expect("Pos(3)");
        assert_eq!(f.contents, "012", "Pos(3)");
        println!("{:?}", sourcemap);
        let f = sourcemap.source(Pos(4)).expect("Pos(4)");
        assert_eq!(f.contents, "345", "Pos(4)");
        let f = sourcemap.source(Pos(6)).expect("Pos(6)");
        assert_eq!(f.contents, "345", "Pos(6)");
        assert_eq!(sourcemap.source(Pos(7)), None);
    }

    #[test]
    fn test_source_shift_pos() {
        let mut sourcemap = SourceMap::new();
        sourcemap.add_dummy("012".to_string());
        sourcemap.add_dummy("345".to_string());
        let f = sourcemap.source(Pos(5)).unwrap();
        assert_eq!(f.shift(Pos(0)), Pos(4));
        assert_eq!(f.shift(Pos(1)), Pos(5));
        assert_eq!(f.shift(Pos(2)), Pos(6));
    }

    #[test]
    fn test_source_get_line() {
        let mut sourcemap = SourceMap::new();
        let input = r#"line 1
            line 2
            line 3
            line 4
        "#;
        sourcemap.add_dummy(input.to_string());
        assert_eq!(sourcemap.line(Pos(0)), None);
        assert_eq!(sourcemap.line(Pos(1)), Some("line 1"));
        assert_eq!(sourcemap.line(Pos(2)), Some("line 1"));
        assert_eq!(sourcemap.line(Pos(7)), Some("line 1"));
        assert_eq!(sourcemap.line(Pos(8)), Some("            line 2"));
    }

    #[test]
    fn test_source_position() {
        let mut sourcemap = SourceMap::new();
        let input = r#"line 1
            line 2
            line 3
            line 4
        "#;
        sourcemap.add_dummy(input.to_string());
        assert_eq!(sourcemap.position(Pos(0)), None);
        assert_eq!(sourcemap.position(Pos(1)),
                   Some(Position {
                            filename: Rc::new("<dummy>".to_string()),
                            line: Line(1),
                            column: Column(1),
                            absolute: Byte(0),
                        }));
        assert_eq!(sourcemap.position(Pos(4)),
                   Some(Position {
                            filename: Rc::new("<dummy>".to_string()),
                            line: Line(1),
                            column: Column(4),
                            absolute: Byte(3),
                        }));
        assert_eq!(sourcemap.position(Pos(7)),
                   Some(Position {
                            filename: Rc::new("<dummy>".to_string()),
                            line: Line(1),
                            column: Column(7),
                            absolute: Byte(6),
                        }));
        assert_eq!(sourcemap.position(Pos(8)),
                   Some(Position {
                            filename: Rc::new("<dummy>".to_string()),
                            line: Line(2),
                            column: Column(1),
                            absolute: Byte(7),
                        }));
    }
}
