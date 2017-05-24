pub mod ir;
mod typing2;

use self::ir::{Expr, ExprKind, Type};
use ast::{NodeId, Expr as AstExpr, ExprKind as AstExprKind};
pub use self::typing2::TypeMap;

error_chain! {
    types {
        Error, ErrorKind, ResultExt, Result;
    }

    errors {
        InvalidTypeUnification(t1: Type, t2: Type) {
            description("invalid type unification")
            display("cannot unify types: {:?} and {:?}", t1, t2)
        }

        CannotInfer(x: NodeId) {
            description("cannot infer type")
            display("cannot infer type: {:?}", x)
        }
    }
}

pub fn type_check(expr: &AstExpr) -> Result<TypeMap<Type>> {
    let mut ctx = typing2::Context::new();
    let mut subst = typing2::Substitution::new();
    ctx.forward_expr(&mut subst, expr)?;
    ctx.determine_types(&subst)
}

pub fn transform(expr: &AstExpr, typemap: &TypeMap<Type>) -> Result<Expr> {
    match expr.kind {
        AstExprKind::Int(n) => Ok(Expr::new(ExprKind::Int(n), Type::Int)),
        AstExprKind::Float(n) => Ok(Expr::new(ExprKind::Float(n), Type::Float)),
        AstExprKind::Add(ref l, ref r) => {
            let l = transform(l, typemap)?;
            let r = transform(r, typemap)?;
            let ty = typemap.get(expr);
            match *ty {
                Type::Int => Ok(Expr::new(ExprKind::AddInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::AddFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Sub(ref l, ref r) => {
            let l = transform(l, typemap)?;
            let r = transform(r, typemap)?;
            let ty = typemap.get(expr);
            match *ty {
                Type::Int => Ok(Expr::new(ExprKind::SubInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::SubFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Mul(ref l, ref r) => {
            let l = transform(l, typemap)?;
            let r = transform(r, typemap)?;
            let ty = typemap.get(expr);
            match *ty {
                Type::Int => Ok(Expr::new(ExprKind::MulInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::MulFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Div(ref l, ref r) => {
            let l = transform(l, typemap)?;
            let r = transform(r, typemap)?;
            let ty = typemap.get(expr);
            match *ty {
                Type::Int => Ok(Expr::new(ExprKind::DivInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::DivFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        _ => unimplemented!(),
    }
}
