use std::collections::HashMap;
use std::fmt;
use std::ops::{Drop, Deref, DerefMut};

use basis::sourcemap::{Spanned, Span, NSPAN};
use basis::errors::Error as BasisError;

use syntax::ast::{Toplevel, ToplevelKind, Def as AstDef, Node as AstNode, NodeKind as AstNodeKind,
                  Let as AstLet};

use typed_ast::{Module, Node, NodeKind, Type, Decl, DeclKind, Let, Param, Def};
use type_env::TypeEnv;
use super::Result as InferResult;
use errors::Error;

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

    fn current_scope(&mut self) -> &mut Env {
        self.envs.last_mut().unwrap_or(&mut self.global_env)
    }

    fn collect_forward_def(&mut self, def: &AstDef, span: Span) -> InferResult<Type> {
        let ret = def.ret
            .as_ref()
            .map(|typ| self.tyenv.convert(typ))
            .unwrap_or(Ok(Type::Unit))
            .map_err(|err| BasisError::span(span, err))?;
        let params = def.params
            .iter()
            .map(|param| {
                     self.tyenv
                         .convert(&param.typ)
                         .map_err(|err| BasisError::span(param.span, err))
                 })
            .collect::<InferResult<_>>()?;
        let ftyp = Type::new_fun(params, ret);
        self.global_env
            .define(def.name.clone(), Spanned::span(span, ftyp.clone()))?;
        Ok(ftyp)
    }

    fn collect_forward_declarations(&mut self,
                                    decls: Vec<Toplevel>)
                                    -> InferResult<Vec<(Type, Toplevel)>> {
        let mut results = Vec::new();
        for decl in decls {
            let typ = match &decl.kind {
                &ToplevelKind::Def(ref def) => self.collect_forward_def(def, decl.span)?,
                &ToplevelKind::Let(ref let_) => {
                    let typ = let_.typ
                        .as_ref()
                        .expect("global `let` should have type specification");
                    let typ = self.tyenv
                        .convert(&typ.value)
                        .map_err(|err| BasisError::span(typ.span, err))?;
                    self.global_env
                        .define(let_.name.clone(), Spanned::span(decl.span, typ.clone()))?;
                    typ
                }
            };
            results.push((typ, decl))
        }
        Ok(results)
    }

    fn lookup_symbol(&self, name: &str) -> Result<Spanned<Type>, Error> {
        for env in self.envs.iter().rev().chain(Some(&self.global_env)) {
            if let Some(sptyp) = env.table.get(name) {
                return Ok(sptyp.cloned());
            }
        }
        bail!("undefined symbol: {:?}", name)
    }

    fn unify(&self, expected: &Type, actual: &Type) -> Result<(), Error> {
        match (expected, actual) {
            (&Type::Var(ref rvar), _) => {
                if rvar.borrow().is_none() {
                    *rvar.borrow_mut() = Some(actual.clone());
                    return Ok(());
                } else {
                    return self.unify(rvar.borrow().as_ref().unwrap(), actual);
                }
            }
            (_, &Type::Var(ref lvar)) => {
                if lvar.borrow().is_none() {
                    *lvar.borrow_mut() = Some(expected.clone());
                    return Ok(());
                } else {
                    return self.unify(expected, lvar.borrow().as_ref().unwrap());
                }
            }
            (&Type::Ref(ref l_inner), &Type::Ref(ref r_inner)) => self.unify(l_inner, r_inner),
            (&Type::Fun(ref lfun), &Type::Fun(ref rfun)) => {
                for (lparam, rparam) in lfun.0.iter().zip(rfun.0.iter()) {
                    self.unify(lparam, rparam)?;
                }
                self.unify(&lfun.1, &rfun.1)?;
                Ok(())
            }
            (_, _) => {
                if expected == actual {
                    Ok(())
                } else {
                    bail!("mismatched types: {} and {}", expected, actual)
                }
            }
        }
    }

    fn process_node_without_check<'a>(&mut self,
                                      node: AstNode,
                                      expect: &Expect<'a>)
                                      -> InferResult<Node> {
        match node.kind {
            AstNodeKind::Unit => {
                Ok(Node {
                       typ: Type::Unit,
                       kind: NodeKind::Unit,
                       span: node.span,
                   })
            }
            AstNodeKind::Int(n) => {
                Ok(Node {
                       typ: Type::Int,
                       kind: NodeKind::Int(n),
                       span: node.span,
                   })
            }
            AstNodeKind::Float(n) => {
                Ok(Node {
                       typ: Type::Float,
                       kind: NodeKind::Float(n),
                       span: node.span,
                   })
            }
            AstNodeKind::Bool(n) => {
                Ok(Node {
                       typ: Type::Bool,
                       kind: NodeKind::Bool(n),
                       span: node.span,
                   })
            }
            AstNodeKind::Parens(e) => self.process_node(*e, expect),
            AstNodeKind::Ref(e) => {
                let inner = self.process_node(*e, &Expect::None)?;
                Ok(Node {
                       typ: Type::new_ref(inner.typ.clone()),
                       kind: NodeKind::Ref(Box::new(inner)),
                       span: node.span,
                   })
            }
            AstNodeKind::Deref(e) => {
                let inner_typ = Type::new_var();
                let inner = self.process_node(*e, &Expect::WithMessage{
                    typ: Type::new_ref(inner_typ.clone()),
                    message: format_args!("cannot dereference non-reference value"),
                })?;
                Ok(Node {
                       typ: inner_typ,
                       kind: NodeKind::Deref(Box::new(inner)),
                       span: node.span,
                   })
            }
            AstNodeKind::Block(mut e) => {
                let mut scoped = self.enter_scope();
                match e.pop() {
                    None => {
                        Ok(Node {
                               typ: Type::Unit,
                               kind: NodeKind::Unit,
                               span: node.span,
                           })
                    }
                    Some(last) => {
                        let stmts = e.into_iter()
                            .map(|e| scoped.process_node(e, &Expect::None))
                            .collect::<InferResult<Vec<_>>>()?;
                        let last = scoped.process_node(last, expect)?;
                        Ok(Node {
                               typ: last.typ.clone(),
                               kind: NodeKind::Block(stmts, Box::new(last)),
                               span: node.span,
                           })
                    }
                }
            }
            AstNodeKind::If(cond, then, els) => {
                let cond =
                    self.process_node(*cond,
                                      &Expect::WithMessage {
                                          typ: Type::Bool,
                                          message: format_args!("condition of 'if' expression"),
                                      })?;
                match els {
                    None => {
                        let then = self.process_node(*then, &Expect::Type { typ: Type::Unit })?;
                        Ok(Node {
                               typ: Type::Unit,
                               kind: NodeKind::If(Box::new(cond), Box::new(then), None),
                               span: node.span,
                           })
                    }
                    Some(els) => {
                        let then = self.process_node(*then, &Expect::None)?;
                        let els = self.process_node(*els, &Expect::WithSpan {
                            typ: then.typ.clone(),
                            message: format_args!("'then' clause and 'else' clause have incompatible types"),
                            span: node.span,
                        })?;
                        Ok(Node {
                               typ: els.typ.clone(),
                               kind: NodeKind::If(Box::new(cond),
                                                  Box::new(then),
                                                  Some(Box::new(els))),
                               span: node.span,
                           })
                    }
                }
            }
            _ => unimplemented!(),
        }
    }

    fn process_node<'a>(&mut self, node: AstNode, expect: &Expect<'a>) -> InferResult<Node> {
        let node = self.process_node_without_check(node, expect)?;
        if let Some(expected_typ) = expect.typ() {
            if let Err(err) = self.unify(expected_typ, &node.typ) {
                let mut err = BasisError::span(node.span, err);
                expect.add_note_to_error(&mut err);
                return Err(err);
            }
        }
        Ok(node)
    }

    fn process_toplevel_def(&mut self,
                            declare_typ: Type,
                            def: AstDef,
                            span: Span)
                            -> InferResult<(String, Decl)> {
        let mut scoped = self.enter_scope();
        let mut typed_params = Vec::new();
        for param in def.params {
            let typ = scoped
                .tyenv
                .convert(&param.typ)
                .map_err(|err| BasisError::span(param.span, err))?;
            scoped
                .current_scope()
                .define(param.name.clone(), Spanned::span(param.span, typ.clone()))?;
            typed_params.push(Param {
                                  name: param.name,
                                  typ: typ,
                                  span: param.span,
                              });
        }
        let declared_ret = def.ret
            .as_ref()
            .map(|typ| scoped.tyenv.convert(typ))
            .unwrap_or(Ok(Type::Unit))
            .map_err(|err| BasisError::span(span, err))?;
        let msg = format_args!("return type of function, declared here");
        let expect = Expect::WithSpan {
            typ: declared_ret.clone(),
            message: msg,
            span: span,
        };
        let node = scoped.process_node(def.body, &expect)?;
        let kind = DeclKind::Def(Box::new(Def {
                                              name: def.name.clone(),
                                              params: typed_params,
                                              ret: declared_ret,
                                              body: node,
                                          }));
        Ok((def.name,
            Decl {
                kind: kind,
                declare_typ: declare_typ,
                span: span,
            }))
    }

    fn process_toplevel_let(&mut self,
                            declare_typ: Type,
                            let_: AstLet,
                            span: Span)
                            -> InferResult<(String, Decl)> {
        let AstLet { name, value, .. } = let_;
        let expect = Expect::Type { typ: declare_typ.clone() };
        let value = self.process_node(value, &expect)?;
        let kind = DeclKind::Let(Box::new(Let {
                                              name: name.clone(),
                                              typ: declare_typ.clone(),
                                              value: value,
                                          }));
        Ok((name,
            Decl {
                kind: kind,
                declare_typ: declare_typ,
                span: span,
            }))
    }

    fn process_toplevel(&mut self,
                        declare_typ: Type,
                        toplevel: Toplevel)
                        -> InferResult<(String, Decl)> {
        match toplevel.kind {
            ToplevelKind::Def(def) => self.process_toplevel_def(declare_typ, *def, toplevel.span),
            ToplevelKind::Let(let_) => self.process_toplevel_let(declare_typ, *let_, toplevel.span),
        }
    }

    pub fn process(&mut self, modname: String, program: Vec<Toplevel>) -> InferResult<Module> {
        let mut module = Module {
            name: modname,
            decls: HashMap::new(),
        };
        let program = self.collect_forward_declarations(program)?;
        for (typ, decl) in program {
            let (name, decl) = self.process_toplevel(typ, decl)?;
            module.decls.insert(name, decl);
        }
        Ok(module)
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
