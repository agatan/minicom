use std::collections::HashMap;
use std::fmt;
use std::ops::{Drop, Deref, DerefMut};

use basis::sourcemap::{Spanned, Span, NSPAN};
use basis::errors::Error as BasisError;

use typed_ast::Type;
use type_env::TypeEnv;
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
                    Spanned::span(NSPAN, Type::new_fun(vec![Type::Unit], Type::Unit)))
            .unwrap();
        e.define("print_int".into(),
                    Spanned::span(NSPAN, Type::new_fun(vec![Type::Int], Type::Int)))
            .unwrap();
        e.define("print_float".into(),
                    Spanned::span(NSPAN, Type::new_fun(vec![Type::Float], Type::Float)))
            .unwrap();
        e.define("print_bool".into(),
                    Spanned::span(NSPAN, Type::new_fun(vec![Type::Bool], Type::Bool)))
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

#[derive(Debug)]
enum Expect<'a> {
    None,
    Type { typ: Type },
    WithMessage {
        typ: Type,
        message: fmt::Arguments<'a>,
    },
    WithSpan {
        typ: Type,
        message: fmt::Arguments<'a>,
        span: Span,
    },
}

impl<'a> Expect<'a> {
    fn typ(&self) -> Option<&Type> {
        match *self {
            Expect::Type { ref typ } => Some(typ),
            Expect::WithMessage { ref typ, .. } => Some(typ),
            Expect::WithSpan { ref typ, .. } => Some(typ),
            Expect::None => None,
        }
    }

    fn message(&self) -> Option<fmt::Arguments<'a>> {
        match *self {
            Expect::WithMessage { message, .. } => Some(message),
            Expect::WithSpan { message, .. } => Some(message),
            _ => None,
        }
    }

    fn span(&self) -> Option<Span> {
        match *self {
            Expect::WithSpan { span, .. } => Some(span),
            _ => None,
        }
    }

    fn add_note_to_error<E>(&self, err: &mut BasisError<E>) {
        if let Some(msg) = self.message() {
            match self.span() {
                Some(span) => note_in!(err, span, "{}", msg),
                None => note!(err, "{}", msg),
            }
        }
    }
}

#[derive(Debug)]
pub struct Infer {
    tyenv: TypeEnv,
    global_env: Env,
    envs: Vec<Env>,
}

impl Infer {
    pub fn new() -> Self {
        Infer {
            tyenv: TypeEnv::new(),
            global_env: Env::with_prelude(),
            envs: Vec::new(),
        }
    }

    fn enter_scope(&mut self) -> Scope {
        self.envs.push(Env::new());
        Scope(self)
    }

    fn exit_scope(&mut self) {
        self.envs.pop().expect("exit from global scope");
    }
}

struct Scope<'a>(&'a mut Infer);

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        self.0.exit_scope();
    }
}

impl<'a> Deref for Scope<'a> {
    type Target = Infer;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}
