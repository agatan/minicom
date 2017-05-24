use super::instr::Instruction;

use sem::ir::{Expr, ExprKind};

pub fn compile_expression(expr: &Expr) -> Vec<Instruction> {
    match expr.kind {
        ExprKind::Int(n) => vec![Instruction::PushInt(n)],
        ExprKind::Float(n) => vec![Instruction::PushFloat(n)],
        ExprKind::AddInt(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::AddInt);
            r_instrs
        }
        ExprKind::SubInt(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::SubInt);
            r_instrs
        }
        ExprKind::MulInt(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::MulInt);
            r_instrs
        }
        ExprKind::DivInt(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::DivInt);
            r_instrs
        }
        ExprKind::AddFloat(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::AddFloat);
            r_instrs
        }
        ExprKind::SubFloat(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::SubFloat);
            r_instrs
        }
        ExprKind::MulFloat(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::MulFloat);
            r_instrs
        }
        ExprKind::DivFloat(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::DivFloat);
            r_instrs
        }
        ExprKind::Print(ref e) => {
            let mut instrs = compile_expression(e);
            instrs.push(Instruction::Print);
            instrs
        }
    }
}
