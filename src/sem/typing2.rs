use std::collections::HashMap;
use std::collections::hash_map::Iter;
use std::convert::From;

use sem::{Error, ErrorKind, Result};
use sem::ir::Type;
use ast::{NodeId, Node, Expr, ExprKind, Type as AstType, TypeKind};

#[derive(Debug)]
pub struct TypeMap<T> {
    table: HashMap<NodeId, T>,
}

impl<T> TypeMap<T> {
    pub fn new() -> Self {
        TypeMap { table: HashMap::new() }
    }

    pub fn insert_node<N: Node>(&mut self, node: &N, v: T) {
        self.insert(node.get_id(), v);
    }

    pub fn insert(&mut self, id: NodeId, v: T) {
        self.table.insert(id, v);
    }

    pub fn iter(&self) -> Iter<NodeId, T> {
        self.table.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVariable(u32);

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
pub struct Substitution {
    table: HashMap<TypeVariable, TypeOrVar>,
    next_id: u32,
}

impl Substitution {
    pub fn new() -> Self {
        Substitution {
            table: HashMap::new(),
            next_id: 0,
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

    fn lookup(&self, x: TypeVariable) -> Option<TypeOrVar> {
        self.table.get(&x).cloned()
    }

    fn apply(&self, ty: TypeOrVar) -> TypeOrVar {
        match ty {
            x @ TypeOrVar::Type(_) => x,
            TypeOrVar::Variable(x) => {
                match self.lookup(x) {
                    Some(ty) => self.apply(ty),
                    None => TypeOrVar::Variable(x),
                }
            }
        }
    }

    fn deref(&self, ty: TypeOrVar) -> Option<Type> {
        match ty {
            TypeOrVar::Type(ty) => Some(ty),
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
        self.map.insert_node(e, ty.clone());
        match e.kind {
            ExprKind::Int(_) => subst.unify(ty, Type::Int.into()),
            ExprKind::Float(_) => subst.unify(ty, Type::Float.into()),
            ExprKind::Add(ref l, ref r) |
            ExprKind::Sub(ref l, ref r) |
            ExprKind::Mul(ref l, ref r) |
            ExprKind::Div(ref l, ref r) => {
                self.forward_expr(subst, l)?;
                self.forward_expr(subst, r)?;
                let lty = self.transform_type(subst, &l.typ);
                let rty = self.transform_type(subst, &r.typ);
                subst.unify(lty.clone(), rty)?;
                subst.unify(ty, lty)
            }
            ExprKind::Parens(ref e) => {
                let inner_ty = self.transform_type(subst, &e.typ);
                subst.unify(ty, inner_ty)?;
                self.forward_expr(subst, e)
            }
        }
    }

    pub fn determine_types(&self, subst: &Substitution) -> Result<TypeMap<Type>> {
        let mut newmap = TypeMap::new();
        for (&id, typ) in self.map.iter() {
            let newtyp = subst.deref(typ.clone()).ok_or(Error::from(ErrorKind::CannotInfer(id)))?;
            newmap.insert(id, newtyp);
        }
        Ok(newmap)
    }
}
