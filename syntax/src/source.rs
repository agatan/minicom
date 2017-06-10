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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn from_file() {
        let result = Source::from_file("./src/source.rs".to_string()).unwrap();
        assert_eq!(result.path, "./src/source.rs");
        assert!(!result.contents.is_empty());
    }
}
