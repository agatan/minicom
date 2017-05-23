use super::instr::Instruction;

use sem::ir::{Expr, ExprKind, Type};

pub fn compile_expression(expr: &Expr) -> Vec<Instruction> {
    let Expr { ref kind, ref typ } = *expr;
    match *kind {
        ExprKind::Int(n) => vec![Instruction::PushInt(n)],
        ExprKind::Float(n) => vec![Instruction::PushFloat(n)],
        ExprKind::Add(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            match *typ {
                Type::Int => r_instrs.push(Instruction::AddInt),
                Type::Float => r_instrs.push(Instruction::AddFloat),
                _ => panic!("'+' operands should be int or float"),
            }
            r_instrs
        }
        ExprKind::Sub(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            match *typ {
                Type::Int => r_instrs.push(Instruction::SubInt),
                Type::Float => r_instrs.push(Instruction::SubFloat),
                _ => panic!("'-' operands should be int or float"),
            }
            r_instrs
        }
        ExprKind::Mul(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            match *typ {
                Type::Int => r_instrs.push(Instruction::MulInt),
                Type::Float => r_instrs.push(Instruction::MulFloat),
                _ => panic!("'*' operands should be int or float"),
            }
            r_instrs
        }
        ExprKind::Div(ref l, ref r) => {
            let mut l_instrs = compile_expression(l);
            let mut r_instrs = compile_expression(r);
            r_instrs.append(&mut l_instrs);
            match *typ {
                Type::Int => r_instrs.push(Instruction::DivInt),
                Type::Float => r_instrs.push(Instruction::DivFloat),
                _ => panic!("'/' operands should be int or float"),
            }
            r_instrs
        }
    }
}
