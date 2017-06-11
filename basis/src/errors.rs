use std::convert::Into;
use std::fmt;
use std::cell::Cell;

use ansi_term::{Colour, Style};

use pos::{Source, Span, Spanned, SpanWithSource};

#[derive(Debug)]
pub struct Error<E, N = E> {
    main_error: Spanned<E>,
    notes: Vec<Spanned<N, Option<Span>>>,
}

impl<E, N> Error<E, N> {
    pub fn new<E2: Into<E>>(err: Spanned<E2>) -> Self {
        Error {
            main_error: err.map(|err| err.into()),
            notes: Vec::new(),
        }
    }

    pub fn span<E2: Into<E>>(span: Span, err: E2) -> Self {
        Self::new(Spanned::span(span, err))
    }

    pub fn note_in<N2: Into<N>>(&mut self, span: Span, note: N2) {
        self.notes.push(Spanned::span(Some(span), note.into()));
    }

    pub fn note<N2: Into<N>>(&mut self, note: N2) {
        self.notes.push(Spanned::span(None, note.into()));
    }

    pub fn with_source<'a>(self, source: &'a Source) -> ErrorWithSource<'a, E, N> {
        ErrorWithSource::new(self, source)
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

thread_local! {
    pub static BOLD_STYLE: Cell<Style> = Cell::new(Style::default().bold());
    pub static ERROR_STYLE: Cell<Style> = Cell::new(Colour::Red.bold());
    pub static NOTE_STYLE: Cell<Style> = Cell::new(Colour::Cyan.bold());
    pub static UNDERLINE_STYLE: Cell<Style> = Cell::new(Style::default().underline());
}

impl<'a, E: fmt::Display, N: fmt::Display> fmt::Display for ErrorWithSource<'a, E, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let span_with_source = format!("{}:",
                                       SpanWithSource::new(self.error.main_error.span,
                                                           self.source));
        writeln!(f,
                 "{} {} {}",
                 BOLD_STYLE.with(|s| s.get().paint(span_with_source)),
                 ERROR_STYLE.with(|s| s.get().paint("error:")),
                 BOLD_STYLE.with(|s| s.get().paint(self.error.main_error.value.to_string())))?;
        if let Some(line) =
            UNDERLINE_STYLE.with(|s| self.source.get_span(self.error.main_error.span, s.get())) {
            writeln!(f, "    {}", line)?;
        }
        for note in self.error.notes.iter() {
            match note.span {
                Some(span) => {
                    let span = format!("{}:", SpanWithSource::new(span, self.source));
                    write!(f, "  {}", BOLD_STYLE.with(|s| s.get().paint(span)))?
                }
                None => write!(f, "  {}", BOLD_STYLE.with(|s| s.get().paint(":")))?,
            }
            writeln!(f,
                     " {} {}",
                     NOTE_STYLE.with(|s| s.get().paint("note:")),
                     BOLD_STYLE.with(|s| s.get().paint(note.value.to_string())))?;
            if let Some(span) = note.span {
                if let Some(line) = UNDERLINE_STYLE.with(|s| self.source.get_span(span, s.get())) {
                    writeln!(f, "    {}", line)?;
                }
            }
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

    fn reset_attributes() {
        BOLD_STYLE.with(|s| s.set(Style::default()));
        ERROR_STYLE.with(|s| s.set(Style::default()));
        NOTE_STYLE.with(|s| s.set(Style::default()));
        UNDERLINE_STYLE.with(|s| s.set(Style::default()));
    }

    #[test]
    fn only_main_error() {
        reset_attributes();
        let err: Error<&str> = Error::new(Spanned::span(ZERO_SPAN, "main error"));
        let source = dummy_source();
        let expected = "<dummy>:1:0: error: main error\n";
        assert_eq!(format!("{}", ErrorWithSource::new(err, &source)), expected);
    }

    #[test]
    fn error_with_notes() {
        reset_attributes();
        let mut err: Error<&str> = Error::new(Spanned::span(ZERO_SPAN, "main error"));
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
