use std::convert::Into;
use std::fmt::Display;
use std::io::prelude::*;
use std::io;

use pos::{Source, Span, Spanned, SpanWithSource};

#[derive(Debug)]
pub struct Error<E, N = E> {
    error: Spanned<E>,
    notes: Vec<Spanned<N, Option<Span>>>,
}

impl<E, N> Error<E, N> {
    pub fn new<E2: Into<E>>(span: Span, err: E2) -> Self {
        Error {
            error: Spanned::span(span, err.into()),
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

impl<E: Display, N: Display> Error<E, N> {
    pub fn show<W: Write>(&self, mut w: W, source: &Source) -> io::Result<()> {
        writeln!(w,
                 "{}: error: {}",
                 SpanWithSource::new(self.error.span, source),
                 self.error.value)?;
        Ok(())
    }
}
