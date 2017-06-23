use std::io::prelude::*;
use std::io;
use std::fs;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// `Pos` is a light-weight representation of source position.
/// `Pos` can be converted to canonical position with `FileMap`.
pub struct Pos(usize);

/// `NPOS` is a dummy position
pub const NPOS: Pos = Pos(0);

#[derive(Debug, Clone, PartialEq)]
/// `File` is a source file.
pub struct File {
    /// `base` is a base offset of this file in `FileMap`
    base: usize,
    /// `size` is a size of its contents
    size: usize,
    name: String,
    contents: String,
}

impl File {
    pub fn executable_name(&self) -> String {
        let path: &::std::path::Path = self.name.as_ref();
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
}

#[derive(Debug, Clone)]
/// `FileMap` holds source files and convert light-weight `Pos` into canonical positions.
pub struct FileMap {
    /// base offset for the next file
    base: usize,
    files: Vec<Rc<File>>,
}

impl FileMap {
    pub fn new() -> Self {
        FileMap {
            base: 1, // 0 means invalid dummy pos
            files: Vec::new(),
        }
    }

    fn add(&mut self, name: String, contents: String) -> Rc<File> {
        let base = self.base;
        let size = contents.len();
        let file = File {
            base: base,
            size: size,
            name: name,
            contents: contents,
        };
        let rc_file = Rc::new(file);
        self.files.push(rc_file.clone());
        self.base += size;
        rc_file
    }

    pub fn add_file(&mut self, filename: String) -> io::Result<Rc<File>> {
        let mut file = fs::File::open(&filename)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(self.add(filename, contents))
    }

    pub fn add_dummy(&mut self, contents: String) -> Rc<File> {
        self.add("<dummy>".to_string(), contents)
    }

    fn search_files(&self, x: usize) -> Option<Rc<File>> {
        let mut found = None;
        for file in self.files.iter() {
            if file.base > x {
                return found.cloned();
            }
            found = Some(file);
        }
        found.cloned()
    }

    pub fn file(&self, pos: Pos) -> Option<Rc<File>> {
        if pos == NPOS {
            return None;
        }
        if let Some(file) = self.search_files(pos.0) {
            if pos.0 < file.base + file.size {
                return Some(file);
            }
        }
        None
    }
}

#[test]
fn test_filemap() {
    let mut filemap = FileMap::new();
    filemap.add_dummy("012".to_string());
    filemap.add_dummy("345".to_string());
    let f = filemap.file(Pos(1)).expect("Pos(1)");
    assert_eq!(f.contents, "012", "Pos(1)");
    let f = filemap.file(Pos(3)).expect("Pos(3)");
    assert_eq!(f.contents, "012", "Pos(3)");
    println!("{:?}", filemap);
    let f = filemap.file(Pos(4)).expect("Pos(4)");
    assert_eq!(f.contents, "345", "Pos(4)");
    let f = filemap.file(Pos(6)).expect("Pos(6)");
    assert_eq!(f.contents, "345", "Pos(6)");
    assert_eq!(filemap.file(Pos(7)), None);
}

#[test]
fn test_file_shift_pos() {
    let mut filemap = FileMap::new();
    filemap.add_dummy("012".to_string());
    filemap.add_dummy("345".to_string());
    let f = filemap.file(Pos(5)).unwrap();
    assert_eq!(f.shift(Pos(0)), Pos(4));
    assert_eq!(f.shift(Pos(1)), Pos(5));
    assert_eq!(f.shift(Pos(2)), Pos(6));
}
