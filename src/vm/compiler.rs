use super::instr::Instruction;

use ast::Expr;

pub fn compile_expression(expr: Expr) -> Vec<Instruction> {
    match expr {
        Expr::Int(n) => vec![Instruction::PushInt(n)],
    }
}
