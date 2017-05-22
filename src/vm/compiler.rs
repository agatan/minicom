use super::instr::Instruction;

use ast::Expr;

pub fn compile_expression(expr: &Expr) -> Vec<Instruction> {
    match *expr {
        Expr::Int(n) => vec![Instruction::PushInt(n)],
        Expr::Add(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::AddInt);
            r_instrs
        }
        Expr::Sub(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::SubInt);
            r_instrs
        }
        Expr::Mul(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::MulInt);
            r_instrs
        }
        Expr::Div(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            r_instrs.push(Instruction::DivInt);
            r_instrs
        }
    }
}
