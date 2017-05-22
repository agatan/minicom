use super::instr::Instruction;

use ast::{Expr, ExprKind};

pub fn compile_expression(expr: &Expr) -> Vec<Instruction> {
    match expr.kind {
        ExprKind::Int(n) => vec![Instruction::PushInt(n)],
        ExprKind::Add(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::AddInt);
            r_instrs
        }
        ExprKind::Sub(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::SubInt);
            r_instrs
        }
        ExprKind::Mul(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::MulInt);
            r_instrs
        }
        ExprKind::Div(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::DivInt);
            r_instrs
        }
        ExprKind::Parens(ref e) => compile_expression(e),
    }
}
