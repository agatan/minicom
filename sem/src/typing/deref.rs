use basis::errors::Error as BasisError;

use super::typed_ast::{Node, NodeKind, Decl, DeclKind, Module};
use Result as InferResult;

pub fn deref_node(node: &mut Node) -> InferResult<()> {
    let span = node.span;
    node.typ = node.typ.deref().map_err(
        |err| BasisError::span(node.span, err),
    )?;
    match node.kind {
        NodeKind::Call(_, ref mut args) => {
            for arg in args.iter_mut() {
                deref_node(arg)?;
            }
        }
        NodeKind::Infix(ref mut lhs, _, ref mut rhs) => {
            deref_node(lhs)?;
            deref_node(rhs)?;
        }
        NodeKind::Block(ref mut nodes, ref mut last) => {
            for n in nodes.iter_mut() {
                deref_node(n)?;
            }
            deref_node(last)?;
        }
        NodeKind::If(ref mut cond, ref mut then, ref mut els) => {
            deref_node(cond)?;
            deref_node(then)?;
            if let Some(ref mut els) = *els {
                deref_node(els)?;
            }
        }
        NodeKind::While(ref mut cond, ref mut body) => {
            deref_node(cond)?;
            deref_node(body)?;
        }
        NodeKind::Ref(ref mut e) => deref_node(e)?,
        NodeKind::Deref(ref mut e) => deref_node(e)?,
        NodeKind::Assign(ref mut to, ref mut value) => {
            deref_node(to)?;
            deref_node(value)?;
        }
        NodeKind::Let(ref mut let_) => {
            let_.typ = let_.typ.deref().map_err(|err| BasisError::span(span, err))?;
            deref_node(&mut let_.value)?;
        }
        NodeKind::Unit |
        NodeKind::Int(_) |
        NodeKind::Float(_) |
        NodeKind::Bool(_) |
        NodeKind::Ident(_) => {}
    }
    Ok(())
}

pub fn deref_decl(decl: &mut Decl) -> InferResult<()> {
    let span = decl.span;
    match decl.kind {
        DeclKind::Def(ref mut def) => {
            for param in def.params.iter_mut() {
                param.typ = param.typ.deref().map_err(|err| BasisError::span(span, err))?;
            }
            def.ret = def.ret.deref().map_err(|err| BasisError::span(span, err))?;
            deref_node(&mut def.body)?;
        }
        DeclKind::Let(ref mut let_) => {
            let_.typ = let_.typ.deref().map_err(|err| BasisError::span(span, err))?;
            deref_node(&mut let_.value)?;
        }
    }
    decl.declare_typ = decl.declare_typ.deref().map_err(|err| {
        BasisError::span(decl.span, err)
    })?;
    Ok(())
}

pub fn deref(module: &mut Module) -> InferResult<()> {
    for decl in module.decls.values_mut() {
        deref_decl(decl)?;
    }
    Ok(())
}
