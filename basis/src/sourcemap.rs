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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// `Pos` is a light-weight representation of source position.
/// `Pos` can be converted to canonical position with `SourceMap`.
pub struct Pos(usize);

/// `NPOS` is a dummy position
pub const NPOS: Pos = Pos(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: Pos,
    end: Pos,
}

#[derive(Debug, Clone, PartialEq)]
/// `Source` is a source file.
pub struct Source {
    /// `base` is a base offset of this source in `SourceMap`
    base: usize,
    /// `size` is a size of its contents
    size: usize,
    name: Rc<String>,
    contents: String,
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
            .chain(Some(contents.len()))
            .collect::<Vec<usize>>();
        Source {
            base: base,
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
        Pos(pos.0 + self.base)
    }

    pub fn line(&self, pos: Pos) -> Option<(usize, &str)> {
        if pos.0 < self.base || self.base + self.size <= pos.0 {
            return None;
        }
        let offset = pos.0 - self.base;
        let mut newline_start = 0usize;
        for (i, &newline) in self.lines.iter().enumerate() {
            if newline > offset {
                return Some((i, &self.contents[newline_start..newline - 1]));
            }
            newline_start = newline;
        }
        None
    }

    pub fn position(&self, pos: Pos) -> Option<Position> {
        if pos.0 < self.base || self.base + self.size <= pos.0 {
            return None;
        }
        let offset = pos.0 - self.base;
        let mut newline_start = 0usize;
        for (i, &newline) in self.lines.iter().enumerate() {
            if newline > offset {
                let position = Position {
                    filename: self.name.clone(),
                    line: Line(i),
                    column: Column(offset - newline_start + 1),
                    absolute: Byte(offset),
                };
                return Some(position);
            }
            newline_start = newline;
        }
        None
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

    fn add(&mut self, name: String, contents: String) -> Rc<Source> {
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
            if source.base > x {
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
            if pos.0 < source.base + source.size {
                return Some(source);
            }
        }
        None
    }

    pub fn source(&self, pos: Pos) -> Option<Rc<Source>> {
        self.source_ref(pos).cloned()
    }

    pub fn line(&self, pos: Pos) -> Option<(usize, &str)> {
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
        assert_eq!(sourcemap.line(Pos(1)), Some((1, "line 1")));
        assert_eq!(sourcemap.line(Pos(2)), Some((1, "line 1")));
        assert_eq!(sourcemap.line(Pos(7)), Some((1, "line 1")));
        assert_eq!(sourcemap.line(Pos(8)), Some((2, "            line 2")));
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
