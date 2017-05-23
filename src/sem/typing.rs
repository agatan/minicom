use std::collections::HashMap;

use sem::ir::{Type, TypeVariable, Expr, ExprKind};
use sem::{Error, ErrorKind, Result};

use ast::{Expr as AExpr, ExprKind as AExprKind, Type as AType, TypeKind as ATypeKind};

pub fn transform(e: AExpr) -> Result<Expr> {
    let typer = Typer;
    let mut subst = Substitution::new();
    typer.transform_expr(&mut subst, e)
}

struct Typer;

impl Typer {
    fn transform_expr(&self, subst: &mut Substitution, e: AExpr) -> Result<Expr> {
        let mut ty = self.transform_type(subst, e.typ);
        let kind = match e.kind {
            AExprKind::Int(n) => {
                ty = self.unify_type(subst, ty, Type::Int)?;
                ExprKind::Int(n)
            }
            AExprKind::Float(n) => {
                ty = self.unify_type(subst, ty, Type::Float)?;
                ExprKind::Float(n)
            }
            AExprKind::Add(box l, box r) => {
                let l = self.transform_expr(subst, l)?;
                let mut r = self.transform_expr(subst, r)?;
                match l.typ {
                    Type::Int => {
                        r.typ = self.unify_type(subst, r.typ, Type::Int)?;
                        ty = self.unify_type(subst, ty, Type::Int)?;
                    }
                    Type::Float => {
                        r.typ = self.unify_type(subst, r.typ, Type::Float)?;
                        ty = self.unify_type(subst, ty, Type::Float)?;
                    }
                    _ => {
                        r.typ = self.unify_type(subst, l.typ.clone(), r.typ)?;
                        ty = self.unify_type(subst, ty, r.typ.clone())?;
                    }
                }
                ExprKind::Add(box l, box r)
            }
            AExprKind::Sub(box l, box r) => {
                let l = self.transform_expr(subst, l)?;
                let mut r = self.transform_expr(subst, r)?;
                match l.typ {
                    Type::Int => {
                        r.typ = self.unify_type(subst, r.typ, Type::Int)?;
                        ty = self.unify_type(subst, ty, Type::Int)?;
                    }
                    Type::Float => {
                        r.typ = self.unify_type(subst, r.typ, Type::Float)?;
                        ty = self.unify_type(subst, ty, Type::Float)?;
                    }
                    _ => {
                        r.typ = self.unify_type(subst, l.typ.clone(), r.typ)?;
                        ty = self.unify_type(subst, ty, r.typ.clone())?;
                    }
                }
                ExprKind::Sub(box l, box r)
            }
            AExprKind::Mul(box l, box r) => {
                let l = self.transform_expr(subst, l)?;
                let mut r = self.transform_expr(subst, r)?;
                match l.typ {
                    Type::Int => {
                        r.typ = self.unify_type(subst, r.typ, Type::Int)?;
                        ty = self.unify_type(subst, ty, Type::Int)?;
                    }
                    Type::Float => {
                        r.typ = self.unify_type(subst, r.typ, Type::Float)?;
                        ty = self.unify_type(subst, ty, Type::Float)?;
                    }
                    _ => {
                        r.typ = self.unify_type(subst, l.typ.clone(), r.typ)?;
                        ty = self.unify_type(subst, ty, r.typ.clone())?;
                    }
                }
                ExprKind::Mul(box l, box r)
            }
            AExprKind::Div(box l, box r) => {
                let l = self.transform_expr(subst, l)?;
                let mut r = self.transform_expr(subst, r)?;
                match l.typ {
                    Type::Int => {
                        r.typ = self.unify_type(subst, r.typ, Type::Int)?;
                        ty = self.unify_type(subst, ty, Type::Int)?;
                    }
                    Type::Float => {
                        r.typ = self.unify_type(subst, r.typ, Type::Float)?;
                        ty = self.unify_type(subst, ty, Type::Float)?;
                    }
                    _ => {
                        r.typ = self.unify_type(subst, l.typ.clone(), r.typ)?;
                        ty = self.unify_type(subst, ty, r.typ.clone())?;
                    }
                }
                ExprKind::Div(box l, box r)
            }
            AExprKind::Parens(box e) => return self.transform_expr(subst, e),
        };
        Ok(Expr::new(kind, ty))
    }

    fn transform_type(&self, subst: &mut Substitution, ty: AType) -> Type {
        match &ty.kind {
            &ATypeKind::Int => Type::Int,
            &ATypeKind::Float => Type::Float,
            &ATypeKind::Hole => subst.new_var(),
        }
    }

    fn unify_type(&self, subst: &mut Substitution, t1: Type, t2: Type) -> Result<Type> {
        let (t1, t2) = (subst.apply(t1), subst.apply(t2));
        match (t1, t2) {
            (Type::Int, Type::Int) => Ok(Type::Int),
            (Type::Float, Type::Float) => Ok(Type::Int),
            (Type::Variable(x), t2) => {
                subst.insert(x, t2.clone());
                Ok(t2)
            }
            (t1, Type::Variable(x)) => {
                subst.insert(x, t1.clone());
                Ok(t1)
            }
            (t1, t2) => Err(ErrorKind::InvalidTypeUnification(t1, t2).into()),
        }
    }
}

#[derive(Debug)]
struct Substitution<'a> {
    parent: Option<&'a Substitution<'a>>,
    table: HashMap<TypeVariable, Type>,
    next_id: u32,
}

impl<'a> Substitution<'a> {
    fn new() -> Self {
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

    fn new_var(&mut self) -> Type {
        let ty = Type::from(TypeVariable::new(self.next_id));
        self.next_id += 1;
        ty
    }

    fn insert(&mut self, x: TypeVariable, ty: Type) {
        self.table.insert(x, ty);
    }

    fn lookup(&self, x: TypeVariable) -> Result<Type> {
        if let Some(ty) = self.table.get(&x) {
            return Ok(ty.clone());
        }
        if let Some(parent) = self.parent {
            parent.lookup(x)
        } else {
            Err(ErrorKind::NonDeterministicTypeVariable(x).into())
        }
    }

    fn apply(&self, ty: Type) -> Type {
        match ty {
            Type::Int => Type::Int,
            Type::Float => Type::Float,
            Type::Variable(x) => {
                match self.lookup(x) {
                    Ok(ty) => self.apply(ty),
                    Err(_) => Type::from(x),
                }
            }
        }
    }

    fn deref(&self, ty: Type) -> Result<Type> {
        match ty {
            Type::Int => Ok(Type::Int),
            Type::Float => Ok(Type::Float),
            Type::Variable(x) => self.lookup(x).and_then(|ty| self.deref(ty)),
        }
    }
}

#[test]
fn test_subst() {
    let mut parent = Substitution::new();
    parent.insert(TypeVariable::new(0), Type::Int);
    parent.insert(TypeVariable::new(1), Type::from(TypeVariable::new(0)));
    assert_eq!(parent.deref(Type::from(TypeVariable::new(1))).unwrap(),
               Type::Int);
    {
        let mut child = parent.scoped();
        child.insert(TypeVariable::new(3), Type::Int);
        child.insert(TypeVariable::new(4), Type::from(TypeVariable::new(1)));
        assert_eq!(child.deref(Type::from(TypeVariable::new(3))).unwrap(),
                   Type::Int);
        assert_eq!(child.deref(Type::from(TypeVariable::new(4))).unwrap(),
                   Type::Int);
    }
}

#[cfg(tests)]
mod tests {
    use super::*;
    use sem::ir::{Type, TypeVariable, Expr, ExprKind};
    use sem::{Error, ErrorKind, Result};

    use ast::{Expr as AExpr, ExprKind as AExprKind, Type as AType, TypeKind as ATypeKind};

    fn test_transform_expr() {
        let e1 = AExpr::new(AExprKind::Int(1));
        let e2 = AExpr::new(AExprKind::Int(2));
        let e_add = AExpr::new(AExprKind::Add(box e1, box e2));
        {
            let e = transform(e_add.clone()).unwrap();
            assert_eq!(e.typ, Type::Int);
        }
        {
            let e = AExpr::new(AExprKind::Sub(box e_add.clone(), box e_add.clone()));
            let e = tranform(e).unwrap();
            assert_eq!(e.typ, Type::Int);
        }
    }
}
