use ast::*;

fn transform_type(typ: Type, expected: &TypeKind) -> Type {
    if typ.is(expected) {
        return typ;
    }
    if typ.is(&TypeKind::Hole) {
        return expected.clone().into();
    }
    unreachable!()
}

pub fn transform(expr: Expr) -> Expr {
    let Expr { kind, mut typ } = expr;
    match kind {
        ExprKind::Int(_) => Expr::with_typ(kind, transform_type(typ, &TypeKind::Int)),
        ExprKind::Add(box l, box r) => {
            let mut new_l = transform(l);
            let mut new_r = transform(r);
            if new_l.typ.is(&TypeKind::Int) || new_r.typ.is(&TypeKind::Int) {
                new_l.typ = transform_type(new_l.typ, &TypeKind::Int);
                new_r.typ = transform_type(new_r.typ, &TypeKind::Int);
                typ = transform_type(typ, &TypeKind::Int);
            } else {
                unreachable!()
            }
            let new_kind = ExprKind::Add(box new_l, box new_r);
            Expr::with_typ(new_kind, typ)
        }
        ExprKind::Sub(box l, box r) => {
            let mut new_l = transform(l);
            let mut new_r = transform(r);
            if new_l.typ.is(&TypeKind::Int) || new_r.typ.is(&TypeKind::Int) {
                new_l.typ = transform_type(new_l.typ, &TypeKind::Int);
                new_r.typ = transform_type(new_r.typ, &TypeKind::Int);
                typ = transform_type(typ, &TypeKind::Int);
            } else {
                unreachable!()
            }
            let new_kind = ExprKind::Sub(box new_l, box new_r);
            Expr::with_typ(new_kind, typ)
        }
        ExprKind::Mul(box l, box r) => {
            let mut new_l = transform(l);
            let mut new_r = transform(r);
            if new_l.typ.is(&TypeKind::Int) || new_r.typ.is(&TypeKind::Int) {
                new_l.typ = transform_type(new_l.typ, &TypeKind::Int);
                new_r.typ = transform_type(new_r.typ, &TypeKind::Int);
                typ = transform_type(typ, &TypeKind::Int);
            } else {
                unreachable!()
            }
            let new_kind = ExprKind::Mul(box new_l, box new_r);
            Expr::with_typ(new_kind, typ)
        }
        ExprKind::Div(box l, box r) => {
            let mut new_l = transform(l);
            let mut new_r = transform(r);
            if new_l.typ.is(&TypeKind::Int) || new_r.typ.is(&TypeKind::Int) {
                new_l.typ = transform_type(new_l.typ, &TypeKind::Int);
                new_r.typ = transform_type(new_r.typ, &TypeKind::Int);
                typ = transform_type(typ, &TypeKind::Int);
            } else {
                unreachable!()
            }
            let new_kind = ExprKind::Div(box new_l, box new_r);
            Expr::with_typ(new_kind, typ)
        }
        ExprKind::Parens(box e) => {
            let mut typ = e.typ.clone();
            let new_e = transform(e);
            typ = transform_type(typ, &*new_e.typ.kind);
            Expr::with_typ(ExprKind::Parens(box new_e), typ)
        }
    }
}
