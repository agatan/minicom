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
    }
}
