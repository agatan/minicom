use std::convert::Into;
use std::fmt;
use std::cell::Cell;

use ansi_term::{Colour, Style};

use sourcemap::{SourceMap, Span, Spanned};

#[derive(Debug)]
pub struct Error<E> {
    main_error: E,
    main_span: Option<Span>,
    notes: Vec<Spanned<String, Option<Span>>>,
}

impl<E> Error<E> {
    pub fn new<E2: Into<E>>(err: Spanned<E2>) -> Self {
        Error {
            main_error: err.value.into(),
            main_span: Some(err.span),
            notes: Vec::new(),
        }
    }

    pub fn from_error<E2: Into<E>>(err: E2) -> Self {
        Error {
            main_error: err.into(),
            main_span: None,
            notes: Vec::new(),
        }
    }

    pub fn span<E2: Into<E>>(span: Span, err: E2) -> Self {
        Self::new(Spanned::span(span, err))
    }

    pub fn assign_span(mut self, span: Span) -> Self {
        if self.main_span.is_none() {
            self.main_span = Some(span);
        }
        self
    }

    pub fn note_in<S: Into<String>>(&mut self, span: Span, note: S) {
        self.notes.push(Spanned::span(Some(span), note.into()));
    }

    pub fn note<S: Into<String>>(&mut self, note: S) {
        self.notes.push(Spanned::span(None, note.into()));
    }

    pub fn with_source_map<'a>(self, sourcemap: &'a SourceMap) -> ErrorWithSource<'a, E> {
        ErrorWithSource::new(self, sourcemap)
    }
}

#[macro_export]
macro_rules! note_in {
    ($err:expr, $span:expr, $msg:expr) => {
        $err.note_in($span, $msg)
    };
    ($err:expr, $span:expr, $fmt:expr, $($args:tt)+) => {
        $err.note_in($span, format!($fmt, $($args)+))
    };
}

#[macro_export]
macro_rules! note {
    ($err:expr, $msg:expr) => {
        $err.note($msg)
    };
    ($err:expr, $fmt:expr, $($args:tt)+) => {
        $err.note(format!($fmt, $($args)+))
    };
}

#[derive(Debug)]
pub struct ErrorWithSource<'a, E> {
    error: Error<E>,
    sourcemap: &'a SourceMap,
}

impl<'a, E> ErrorWithSource<'a, E> {
    pub fn new(error: Error<E>, sourcemap: &'a SourceMap) -> Self {
        ErrorWithSource {
            error: error,
            sourcemap: sourcemap,
        }
    }
}

thread_local! {
    pub static BOLD_STYLE: Cell<Style> = Cell::new(Style::default().bold());
    pub static ERROR_STYLE: Cell<Style> = Cell::new(Colour::Red.bold());
    pub static NOTE_STYLE: Cell<Style> = Cell::new(Colour::Cyan.bold());
    pub static UNDERLINE_STYLE: Cell<Style> = Cell::new(Style::default().underline());
}

impl<'a, E: fmt::Display> fmt::Display for ErrorWithSource<'a, E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let span_with_source = match self.error.main_span {
            Some(span) => format!("{}: ", span.display(self.sourcemap)),
            None => "".to_string(),
        };
        writeln!(
            f,
            "{} {} {}",
            BOLD_STYLE.with(|s| s.get().paint(span_with_source)),
            ERROR_STYLE.with(|s| s.get().paint("error:")),
            BOLD_STYLE.with(|s| s.get().paint(self.error.main_error.to_string()))
        )?;
        if let Some(span) = self.error.main_span {
            if let Some(line) = self.sourcemap.line(span.start) {
                writeln!(f, "    {}", line)?;
            }
        }
        for note in self.error.notes.iter() {
            match note.span {
                Some(span) => {
                    let span = format!("{}:", span.display(self.sourcemap));
                    write!(f, "  {}", BOLD_STYLE.with(|s| s.get().paint(span)))?
                }
                None => write!(f, "  {}", BOLD_STYLE.with(|s| s.get().paint(":")))?,
            }
            writeln!(
                f,
                " {} {}",
                NOTE_STYLE.with(|s| s.get().paint("note:")),
                BOLD_STYLE.with(|s| s.get().paint(note.value.to_string()))
            )?;
            if let Some(span) = note.span {
                if let Some(line) = self.sourcemap.line(span.start) {
                    writeln!(f, "    {}", line)?;
                }
            }
        }
        Ok(())
    }
}

pub fn disable_colorized_error() {
    BOLD_STYLE.with(|s| s.set(Style::default()));
    ERROR_STYLE.with(|s| s.set(Style::default()));
    NOTE_STYLE.with(|s| s.set(Style::default()));
    UNDERLINE_STYLE.with(|s| s.set(Style::default()));
}

#[cfg(test)]
mod tests {
    use super::*;
    use sourcemap::*;

    const ZERO_SPAN: Span = Span {
        start: Pos(1),
        end: Pos(1),
    };

    fn dummy_source_map() -> SourceMap {
        let mut srcmap = SourceMap::new();
        srcmap.add_dummy("dummy".to_string());
        srcmap
    }

    #[test]
    fn only_main_error() {
        disable_colorized_error();
        let err: Error<&str> = Error::new(Spanned::span(ZERO_SPAN, "main error"));
        let source = dummy_source_map();
        let expected = "<dummy>:1:1: error: main error\n    dummy\n";
        assert_eq!(format!("{}", ErrorWithSource::new(err, &source)), expected);
    }

    #[test]
    fn error_with_notes() {
        disable_colorized_error();
        let mut err: Error<&str> = Error::new(Spanned::span(ZERO_SPAN, "main error"));
        note_in!(err, ZERO_SPAN, "spanned {}", "note");
        note!(err, "non-spanned note");
        let source = dummy_source_map();
        let expected = r#"<dummy>:1:1: error: main error
    dummy
  <dummy>:1:1: note: spanned note
    dummy
  : note: non-spanned note
"#;
        assert_eq!(format!("{}", ErrorWithSource::new(err, &source)), expected);
    }
}
