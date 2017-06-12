use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};
use std::fmt;

use basis::pos::{Span, Spanned, DUMMY_SPAN};
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

    fn enter_scope(&mut self) -> Scope {
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

    fn get_declared_global_var(&self, name: &str) -> Type {
        let declared = self.global_env.table.get(name).expect("forward collected");
        match declared.value {
            Entry::Var(ref typ) => typ.clone(),
            _ => panic!("{} is not declared as variable", name),
        }
    }

    fn get_declared_function(&self, name: &str) -> (Vec<Type>, Type) {
        let declared = self.global_env.table.get(name).expect("forward collected");
        match declared.value {
            Entry::Function(ref args, ref ret) => (args.clone(), ret.clone()),
            _ => panic!("{} is not declared as function", name),
        }
    }

    fn unify(&self, expected: &Type, actual: &Type) -> Result<(), Error> {
        if expected != actual {
            bail!("mismatched types: {:?} and {:?}", expected, actual)
        }
        Ok(())
    }

    fn infer_node_without_registeration<'a>(&mut self,
                                            node: &Spanned<Node>,
                                            expect: Option<&Expect<'a>>)
                                            -> SemResult<Type> {
        unimplemented!()
    }

    fn infer_node<'a>(&mut self,
                      node: &Spanned<Node>,
                      expect: Option<&Expect<'a>>)
                      -> SemResult<Type> {
        let actual = self.infer_node_without_registeration(node, expect)?;
        if let Some(ref expect) = expect {
            if let Err(err) = self.unify(&expect.typ, &actual) {
                let mut err = BasisError::span(node.span, err);
                match expect.span {
                    Some(span) => note_in!(err, span, "{}", expect.message),
                    None => note!(err, "{}", expect.message),
                }
                return Err(err);
            }
        }
        self.typemap
            .borrow_mut()
            .insert(node.value.id, actual.clone());
        Ok(actual)
    }

    fn infer_toplevel(&mut self, node: &Spanned<Toplevel>) -> SemResult<()> {
        match node.value.kind {
            ToplevelKind::Def(ref def) => {
                let (declared_args, declared_ret) = self.get_declared_function(&def.name);
                let mut scoped = self.enter_scope();
                let arg_names = def.args.iter().map(|arg| arg.0.clone());
                for (name, typ) in arg_names.into_iter().zip(declared_args) {
                    scoped
                        .current_scope()
                        .define(name, Spanned::span(node.span, Entry::Var(typ)))?;
                }
                let expect = Expect {
                    message: format_args!(""),
                    typ: declared_ret,
                    span: None,
                };
                scoped.infer_node(&def.body, Some(&expect))?;
            }
            ToplevelKind::Let(ref let_) => {
                let declared_typ = self.get_declared_global_var(&let_.name);
                let expect = Expect {
                    message: format_args!(""),
                    typ: declared_typ,
                    span: let_.typ.as_ref().map(|typ| typ.span.clone()),
                };
                self.infer_node(&let_.value, Some(&expect))?;
            }
            ToplevelKind::Expr(ref node) => {
                self.infer_node(node, None)?;
            }
        }
        Ok(())
    }

    pub fn infer_program(&mut self, nodes: &[Spanned<Toplevel>]) -> SemResult<()> {
        self.collect_forward(nodes)?;

        for toplevel in nodes {
            self.infer_toplevel(toplevel)?;
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
                note_in!(err,
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

#[derive(Debug)]
struct Expect<'a> {
    message: fmt::Arguments<'a>,
    typ: Type,
    span: Option<Span>,
}
