use std::collections::HashMap;
use std::convert::From;

use sem::{ErrorKind, Result};
use ast::{NodeId, Node, Expr, ExprKind, Type as AstType, TypeKind};

#[derive(Debug)]
pub struct TypeMap<T> {
    table: HashMap<NodeId, T>,
}

impl<T> TypeMap<T> {
    pub fn new() -> Self {
        TypeMap { table: HashMap::new() }
    }

    pub fn insert<N: Node>(&mut self, node: &N, v: T) {
        self.table.insert(node.get_id(), v);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TypeVariable(u32);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
}

#[derive(Debug, Clone)]
enum TypeOrVar {
    Type(Type),
    Variable(TypeVariable),
}

impl From<Type> for TypeOrVar {
    fn from(t: Type) -> Self {
        TypeOrVar::Type(t)
    }
}

impl From<TypeVariable> for TypeOrVar {
    fn from(x: TypeVariable) -> Self {
        TypeOrVar::Variable(x)
    }
}

#[derive(Debug)]
pub struct Substitution<'a> {
    parent: Option<&'a Substitution<'a>>,
    table: HashMap<TypeVariable, TypeOrVar>,
    next_id: u32,
}

impl<'a> Substitution<'a> {
    pub fn new() -> Self {
        Substitution {
            parent: None,
            table: HashMap::new(),
            next_id: 0,
        }
    }

    fn scoped(&'a self) -> Substitution<'a> {
        Substitution {
            parent: Some(self),
            table: HashMap::new(),
            next_id: self.next_id,
        }
    }

    fn new_var(&mut self) -> TypeOrVar {
        let ty = TypeOrVar::Variable(TypeVariable(self.next_id));
        self.next_id += 1;
        ty
    }

    fn insert(&mut self, x: TypeVariable, ty: TypeOrVar) {
        self.table.insert(x, ty);
    }

    fn lookup(&self, x: TypeVariable) -> Result<TypeOrVar> {
        if let Some(ty) = self.table.get(&x) {
            return Ok(ty.clone());
        }
        if let Some(parent) = self.parent {
            parent.lookup(x)
        } else {
            unimplemented!()
            // Err(ErrorKind::NonDeterministicTypeVariable(x).into())
        }
    }

    fn apply(&self, ty: TypeOrVar) -> TypeOrVar {
        match ty {
            x @ TypeOrVar::Type(_) => x,
            TypeOrVar::Variable(x) => {
                match self.lookup(x) {
                    Ok(ty) => self.apply(ty),
                    Err(_) => TypeOrVar::Variable(x),
                }
            }
        }
    }

    fn deref(&self, ty: TypeOrVar) -> Result<Type> {
        match ty {
            TypeOrVar::Type(ty) => Ok(ty),
            TypeOrVar::Variable(x) => self.lookup(x).and_then(|ty| self.deref(ty)),
        }
    }

    fn unify(&mut self, t1: TypeOrVar, t2: TypeOrVar) -> Result<()> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);
        match (t1, t2) {
            (TypeOrVar::Type(t1), TypeOrVar::Type(t2)) => {
                if t1 == t2 { Ok(()) } else { unimplemented!() }
            }
            (TypeOrVar::Variable(x), t2) => {
                self.insert(x, t2);
                Ok(())
            }
            (t1, TypeOrVar::Variable(x)) => {
                self.insert(x, t1);
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct Context {
    map: TypeMap<TypeOrVar>,
}

impl Context {
    pub fn new() -> Self {
        Context { map: TypeMap::new() }
    }

    fn transform_type(&mut self, subst: &mut Substitution, ty: &AstType) -> TypeOrVar {
        match ty.kind {
            TypeKind::Int => Type::Int.into(),
            TypeKind::Float => Type::Float.into(),
            TypeKind::Hole => subst.new_var(),
        }
    }

    pub fn forward_expr(&mut self, subst: &mut Substitution, e: &Expr) -> Result<()> {
        let ty = self.transform_type(subst, &e.typ);
        self.map.insert(e, ty.clone());
        match e.kind {
            ExprKind::Int(_) => subst.unify(ty, Type::Int.into()),
            ExprKind::Float(_) => subst.unify(ty, Type::Float.into()),
            ExprKind::Add(ref l, ref r) |
            ExprKind::Sub(ref l, ref r) |
            ExprKind::Mul(ref l, ref r) |
            ExprKind::Div(ref l, ref r) => {
                let lty = self.transform_type(subst, &l.typ);
                let rty = self.transform_type(subst, &r.typ);
                subst.unify(lty, rty)
            }
            ExprKind::Parens(ref e) => {
                let inner_ty = self.transform_type(subst, &e.typ);
                subst.unify(ty, inner_ty)?;
                self.forward_expr(subst, e)
            }
        }
    }
}
