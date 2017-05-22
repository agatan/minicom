use super::instr::Instruction;

use ast::Expr;

pub fn compile_expression(expr: &Expr) -> Vec<Instruction> {
    match *expr {
        Expr::Int(n) => vec![Instruction::PushInt(n)],
        Expr::Add(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            l_instrs.append(&mut r_instrs);
            l_instrs.push(Instruction::AddInt);
            l_instrs
        }
    }
}
