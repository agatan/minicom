use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};
use std::fmt;

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

    pub fn collect_forward_declarations(&mut self, decls: &[Spanned<Toplevel>]) -> SemResult<()> {
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

    fn getvar(&self, name: &str) -> Result<Spanned<&Type>, Error> {
        for env in self.envchain.iter().rev().chain(Some(&self.global_env)) {
            match env.table.get(name) {
                Some(spanned) => {
                    match spanned.value {
                        Entry::Function(_, _) => bail!("{:?} is not a variable", name),
                        Entry::Var(ref typ) => return Ok(Spanned::span(spanned.span, typ)),
                    }
                }
                None => {}
            }
        }
        bail!("undefined variable: {:?}", name)
    }

    fn get_function(&self, name: &str) -> Result<Spanned<(Vec<Type>, Type)>, Error> {
        for env in self.envchain.iter().rev().chain(Some(&self.global_env)) {
            match env.table.get(name) {
                Some(spanned) => {
                    match spanned.value {
                        Entry::Function(ref args, ref ret) => {
                            let f = Spanned::span(spanned.span, (args.clone(), ret.clone()));
                            return Ok(f);
                        }
                        Entry::Var(_) => bail!("{:?} is not a function", name),
                    }
                }
                None => {}
            }
        }
        bail!("undefined function: {:?}", name)
    }

    fn unify(&self, expected: &Type, actual: &Type) -> Result<(), Error> {
        if expected != actual {
            bail!("mismatched types: {:?} and {:?}", expected, actual)
        }
        Ok(())
    }

    fn infer_binary_operation<'a>(&mut self,
                                  lhs: &Spanned<Node>,
                                  op: Operator,
                                  rhs: &Spanned<Node>)
                                  -> SemResult<Type> {
        let lhs_typ = self.infer_node(lhs, &Expect::None)?;
        let rhs_typ = self.infer_node(rhs, &Expect::Type { typ: lhs_typ.clone() })?;
        if op.is_arithmetic() {
            Ok(rhs_typ)
        } else {
            Ok(Type::Bool)
        }
    }

    fn infer_node_without_check<'a>(&mut self,
                                    node: &Spanned<Node>,
                                    expect: &Expect<'a>)
                                    -> SemResult<Type> {
        match node.value.kind {
            NodeKind::Unit => Ok(Type::Unit),
            NodeKind::Int(_) => Ok(Type::Int),
            NodeKind::Float(_) => Ok(Type::Float),
            NodeKind::Bool(_) => Ok(Type::Bool),
            NodeKind::Ident(ref name) => {
                self.getvar(name)
                    .map(|typ| typ.value.clone())
                    .map_err(|err| BasisError::span(node.span, err))
            }
            NodeKind::Call(ref fname, ref args) => {
                let spanned_fun = self.get_function(fname)
                    .map_err(|err| BasisError::span(node.span, err))?;
                let (function_args, function_ret) = spanned_fun.value;
                if args.len() != function_args.len() {
                    let msg = format!("invalid number of arguments for {:?}: expected {}, but given {}",
                                      fname,
                                      function_args.len(),
                                      args.len());
                    return Err(BasisError::span(node.span, msg));
                }
                for (arg, expected_arg) in args.iter().zip(function_args) {
                    let expect = Expect::Type { typ: expected_arg };
                    self.infer_node(arg, &expect)?;
                }
                Ok(function_ret)
            }
            NodeKind::Parens(ref e) => self.infer_node(e, expect),
            NodeKind::Print(ref e) => self.infer_node(e, expect),
            NodeKind::Block(ref nodes) => {
                match nodes.split_last() {
                    None => Ok(Type::Unit),
                    Some((last, init)) => {
                        for node in init {
                            self.infer_node(node, &Expect::None)?;
                        }
                        self.infer_node(last, expect)
                    }
                }
            }
            NodeKind::If(ref cond, ref then, ref els) => {
                self.infer_node(cond,
                                &Expect::WithMessage {
                                    typ: Type::Bool,
                                    message: format_args!("condition of 'if' expression"),
                                })?;
                match *els {
                    None => self.infer_node(then, &Expect::Type { typ: Type::Unit }),
                    Some(ref els) => {
                        let then_typ = self.infer_node(then, &Expect::None)?;
                        self.infer_node(els,
                                        &Expect::WithSpan {
                                            typ: then_typ.clone(),
                                            message: format_args!("'if' and 'else' have incompatible types"),
                                            span: node.span,
                                        })
                    }
                }
            }
            NodeKind::While(ref cond, ref body) => {
                self.infer_node(cond,
                                &Expect::WithMessage {
                                    typ: Type::Bool,
                                    message: format_args!("condition of 'while' expression"),
                                })?;
                self.infer_node(body, &Expect::None)?;
                Ok(Type::Unit)
            }
            NodeKind::Let(ref let_) => {
                let typ = match let_.typ {
                    None => self.infer_node(&let_.value, &Expect::None)?,
                    Some(ref typ) => {
                        let typ = self.convert_type(&typ.value)
                            .map_err(|err| BasisError::span(typ.span, err))?;
                        self.infer_node(&let_.value, &Expect::Type { typ: typ })?
                    }
                };
                let var = Spanned::span(node.span, Entry::Var(typ));
                self.current_scope().define(let_.name.clone(), var)?;
                Ok(Type::Unit)
            }
            NodeKind::Assign(ref var, ref value) => {
                let vartyp = self.getvar(var)
                    .map(|sp| sp.map(Type::clone))
                    .map_err(|err| BasisError::span(node.span, err))?;
                self.infer_node(value,
                                &Expect::WithSpan {
                                    typ: vartyp.value,
                                    message: format_args!("variable {:?} is defined here", var),
                                    span: vartyp.span,
                                })?;
                Ok(Type::Unit)
            }
            NodeKind::Infix(ref lhs, op, ref rhs) => self.infer_binary_operation(lhs, op, rhs),
        }
    }

    fn infer_node<'a>(&mut self, node: &Spanned<Node>, expect: &Expect<'a>) -> SemResult<Type> {
        let actual = self.infer_node_without_check(node, expect)?;
        if let Some(expected_typ) = expect.typ() {
            if let Err(err) = self.unify(expected_typ, &actual) {
                let mut err = BasisError::span(node.span, err);
                expect.add_note_to_error(&mut err);
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
                let msg = format_args!("return type of function, declared here");
                let expect = Expect::WithSpan {
                    typ: declared_ret,
                    message: msg,
                    span: node.span,
                };
                scoped.infer_node(&def.body, &expect)?;
            }
            ToplevelKind::Let(ref let_) => {
                let declared_typ = self.get_declared_global_var(&let_.name);
                let expect = Expect::Type { typ: declared_typ };
                self.infer_node(&let_.value, &expect)?;
            }
            ToplevelKind::Expr(ref node) => {
                self.infer_node(node, &Expect::None)?;
            }
        }
        Ok(())
    }

    pub fn infer_program(&mut self, nodes: &[Spanned<Toplevel>]) -> SemResult<()> {
        self.collect_forward_declarations(nodes)?;

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
