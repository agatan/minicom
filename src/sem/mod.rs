pub mod ir;
mod typing;

use self::ir::{Expr, ExprKind, Type};
use ast::{NodeId, Node, Expr as AstExpr, ExprKind as AstExprKind};
pub use self::typing::TypeMap;

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

pub fn type_check(nodes: &[Node]) -> Result<TypeMap<Type>> {
    let mut ctx = typing::Context::new();
    let mut subst = typing::Substitution::new();
    for node in nodes {
        match *node {
            Node::Expr(ref e) => ctx.forward_expr(&mut subst, e)?,
            Node::Stmt(_) => unimplemented!(),
        }
    }
    ctx.determine_types(&subst)
}

fn transform_expr(expr: &AstExpr, typemap: &TypeMap<Type>) -> Result<Expr> {
    match expr.kind {
        AstExprKind::Int(n) => Ok(Expr::new(ExprKind::Int(n), Type::Int)),
        AstExprKind::Float(n) => Ok(Expr::new(ExprKind::Float(n), Type::Float)),
        AstExprKind::Add(ref l, ref r) => {
            let l = transform_expr(l, typemap)?;
            let r = transform_expr(r, typemap)?;
            let ty = typemap.get(expr.id);
            match ty {
                Type::Int => Ok(Expr::new(ExprKind::AddInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::AddFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Sub(ref l, ref r) => {
            let l = transform_expr(l, typemap)?;
            let r = transform_expr(r, typemap)?;
            let ty = typemap.get(expr.id);
            match ty {
                Type::Int => Ok(Expr::new(ExprKind::SubInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::SubFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Mul(ref l, ref r) => {
            let l = transform_expr(l, typemap)?;
            let r = transform_expr(r, typemap)?;
            let ty = typemap.get(expr.id);
            match ty {
                Type::Int => Ok(Expr::new(ExprKind::MulInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::MulFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Div(ref l, ref r) => {
            let l = transform_expr(l, typemap)?;
            let r = transform_expr(r, typemap)?;
            let ty = typemap.get(expr.id);
            match ty {
                Type::Int => Ok(Expr::new(ExprKind::DivInt(Box::new(l), Box::new(r)), Type::Int)),
                Type::Float => {
                    Ok(Expr::new(ExprKind::DivFloat(Box::new(l), Box::new(r)), Type::Float))
                }
            }
        }
        AstExprKind::Parens(ref e) => transform_expr(e, typemap),
        AstExprKind::Print(ref e) => {
            let e = transform_expr(e, typemap)?;
            let ty = e.typ.clone();
            Ok(Expr::new(ExprKind::Print(Box::new(e)), ty))
        }
    }
}

pub fn transform(nodes: &[Node], typemap: &TypeMap<Type>) -> Result<Vec<Expr>> {
    let mut result = Vec::with_capacity(nodes.len());
    for node in nodes {
        match *node {
            Node::Expr(ref e) => result.push(transform_expr(e, typemap)?),
            Node::Stmt(_) => unimplemented!(),
        }
    }
    Ok(result)
}
