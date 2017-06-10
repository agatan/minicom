use std::convert::Into;
use std::fmt;
use std::io::prelude::*;
use std::io;

use pos::{Source, Span, Spanned, SpanWithSource};

#[derive(Debug)]
pub struct Error<E, N = E> {
    main_error: Spanned<E>,
    notes: Vec<Spanned<N, Option<Span>>>,
}

impl<E, N> Error<E, N> {
    pub fn new<E2: Into<E>>(span: Span, err: E2) -> Self {
        Error {
            main_error: Spanned::span(span, err.into()),
            notes: Vec::new(),
        }
    }

    pub fn note_in<N2: Into<N>>(&mut self, span: Span, note: N2) {
        self.notes.push(Spanned::span(Some(span), note.into()));
    }

    pub fn note<N2: Into<N>>(&mut self, note: N2) {
        self.notes.push(Spanned::span(None, note.into()));
    }
}

impl<E: fmt::Display, N: fmt::Display> Error<E, N> {
    pub fn show<W: Write>(&self, mut w: W, source: &Source) -> io::Result<()> {
        writeln!(w,
                 "{}: error: {}",
                 SpanWithSource::new(self.main_error.span, source),
                 self.main_error.value)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct ErrorWithSource<'a, E, N = E> {
    error: Error<E, N>,
    source: &'a Source,
}

impl<'a, E, N> ErrorWithSource<'a, E, N> {
    pub fn new(error: Error<E, N>, source: &'a Source) -> Self {
        ErrorWithSource {
            error: error,
            source: source,
        }
    }
}

impl<'a, E: fmt::Display, N: fmt::Display> fmt::Display for ErrorWithSource<'a, E, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let span_with_source = SpanWithSource::new(self.error.main_error.span, self.source);
        writeln!(f,
                 "{}: error: {}",
                 span_with_source,
                 self.error.main_error.value)?;
        for note in self.error.notes.iter() {
            match note.span {
                Some(span) => write!(f, "  {}:", SpanWithSource::new(span, self.source))?,
                None => write!(f, "  :")?,
            }
            writeln!(f, " note: {}", note.value)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pos::*;

    const ZERO_SPAN: Span = Span {
        start: Location {
            line: Line(0),
            column: Column(0),
            absolute: Byte(0),
        },
        end: Location {
            line: Line(0),
            column: Column(0),
            absolute: Byte(0),
        },
    };

    fn dummy_source() -> Source {
        Source::with_dummy("".to_owned())
    }

    #[test]
    fn only_main_error() {
        let err: Error<&str> = Error::new(ZERO_SPAN, "main error");
        let source = dummy_source();
        let expected = "<dummy>:1:0: error: main error\n";
        assert_eq!(format!("{}", ErrorWithSource::new(err, &source)), expected);
    }

    #[test]
    fn error_with_notes() {
        let mut err: Error<&str> = Error::new(ZERO_SPAN, "main error");
        err.note_in(ZERO_SPAN, "spanned note");
        err.note("non-spanned note");
        let source = dummy_source();
        let expected = r#"<dummy>:1:0: error: main error
  <dummy>:1:0: note: spanned note
  : note: non-spanned note
"#;
        assert_eq!(format!("{}", ErrorWithSource::new(err, &source)), expected);
    }
}
