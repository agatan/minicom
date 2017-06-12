use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Drop;

use basis::pos::{Span, Spanned};
use basis::errors::Error as BasisError;
use syntax::ast::{self, Toplevel, ToplevelKind, Node, NodeKind, Operator};

use sem::typing::TypeMap;
use sem::tyenv::TypeEnv;
use sem::ir::Type;
use sem::{Error, Result as SemResult};

#[derive(Debug)]
pub struct Infer {
    typemap: Rc<RefCell<TypeMap>>,
    tyenv: Rc<TypeEnv>,
    global_env: Env,
    envchain: Vec<Env>,
}

impl Infer {
    pub fn new() -> Self {
        Infer {
            typemap: Rc::new(RefCell::new(TypeMap::new())),
            tyenv: Rc::new(TypeEnv::new()),
            global_env: Env::new(),
            envchain: Vec::new(),
        }
    }

    fn convert_type(&self, ast_typ: &ast::Type) -> Result<Type, Error> {
        self.tyenv.get(&ast_typ.name)
    }

    fn entry_scope(&mut self) -> Scope {
        self.envchain.push(Env::new());
        Scope(self)
    }

    fn exit_scope(&mut self) {
        self.envchain.pop().expect("exit from global scope");
    }

    fn current_scope(&mut self) -> &mut Env {
        self.envchain.last_mut().unwrap_or(&mut self.global_env)
    }

    pub fn collect_forward(&mut self, decls: &[Spanned<Toplevel>]) -> SemResult<()> {
        for decl in decls {
            match decl.value.kind {
                ToplevelKind::Def(ref def) => {
                    let ret_typ = def.ret
                        .as_ref()
                        .map(|typ| self.convert_type(typ))
                        .unwrap_or(Ok(Type::Unit))
                        .map_err(|err| BasisError::span(decl.span, err))?;
                    let args = def.args
                        .iter()
                        .map(|&(_, ref typ)| {
                                 self.convert_type(typ)
                                     .map_err(|err| BasisError::span(decl.span, err))
                             })
                        .collect::<SemResult<Vec<_>>>()?;
                    self.global_env
                        .define(def.name.clone(),
                                Spanned::span(decl.span, Entry::Function(args, ret_typ)))?;
                }
                ToplevelKind::Let(ref let_) => {
                    let typ = let_.typ
                        .as_ref()
                        .expect("global `let` shuold have type specification");
                    let typ = self.convert_type(&typ.value)
                        .map_err(|err| BasisError::span(typ.span, err))?;
                    self.global_env
                        .define(let_.name.clone(), Spanned::span(decl.span, Entry::Var(typ)))?;
                }
                _ => (),
            }
        }
        Ok(())
    }
}

struct Scope<'a>(&'a mut Infer);

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        self.0.exit_scope()
    }
}

#[derive(Debug)]
struct Env {
    pub table: HashMap<String, Spanned<Entry>>,
}

impl Env {
    fn new() -> Self {
        Env { table: HashMap::new() }
    }

    fn define(&mut self, name: String, entry: Spanned<Entry>) -> SemResult<()> {
        use std::collections::hash_map::Entry::*;
        match self.table.entry(name) {
            Vacant(v) => {
                v.insert(entry);
            }
            Occupied(o) => {
                let mut err = BasisError::span(entry.span,
                                               format!("duplicate definition: {:?}", o.key()));
                note_error!(err,
                            o.get().span,
                            "previous definition of {:?} here",
                            o.key());
                return Err(err);
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
enum Entry {
    Function(Vec<Type>, Type),
    Var(Type),
}
