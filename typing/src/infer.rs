use std::collections::HashMap;

use basis::sourcemap::{Spanned, NSPAN};
use basis::errors::Error as BasisError;

use typed_ast::Type;
use super::Result as InferResult;

#[derive(Debug)]
struct Env {
    table: HashMap<String, Spanned<Type>>,
}

impl Env {
    fn new() -> Self {
        Env { table: HashMap::new() }
    }

    fn with_prelude() -> Self {
        let mut e = Self::new();
        e.define("print_unit".into(),
                    Spanned::span(NSPAN, Type::fun(vec![Type::Unit], Type::Unit)))
            .unwrap();
        e.define("print_int".into(),
                    Spanned::span(NSPAN, Type::fun(vec![Type::Int], Type::Int)))
            .unwrap();
        e.define("print_float".into(),
                    Spanned::span(NSPAN, Type::fun(vec![Type::Float], Type::Float)))
            .unwrap();
        e.define("print_bool".into(),
                    Spanned::span(NSPAN, Type::fun(vec![Type::Bool], Type::Bool)))
            .unwrap();
        e
    }

    fn define(&mut self, name: String, entry: Spanned<Type>) -> InferResult<()> {
        use std::collections::hash_map::Entry::*;
        match self.table.entry(name) {
            Vacant(v) => {
                v.insert(entry);
            }
            Occupied(o) => {
                let mut err = BasisError::span(entry.span,
                                               format!("duplicate definition: {:?}", o.key()));
                if o.get().span != NSPAN {
                    note_in!(err,
                             o.get().span,
                             "previous definition of {:?} here",
                             o.key());
                }
                return Err(err);
            }
        }
        Ok(())
    }
}
