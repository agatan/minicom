pub mod instr;
pub mod compiler;
pub mod value;
pub mod stack;

use ast;
pub use self::value::Value;
use self::instr::Instruction;
use self::instr::Instruction::*;

struct VM<'a> {
    instrs: &'a [Instruction],
    pc: usize,
    stack: stack::Stack,
}

impl<'a> VM<'a> {
    fn new(instrs: &'a [Instruction]) -> Self {
        VM {
            instrs: instrs,
            pc: 0,
            stack: stack::Stack::new(),
        }
    }

    fn fetch_instr(&mut self) -> Instruction {
        let ins = self.instrs[self.pc];
        debug!("fetched instruction: {:?}", ins);
        self.pc += 1;
        ins
    }

    fn eval_instr(&mut self, ins: Instruction) {
        match ins {
            PushInt(n) => {
                let v = Value::Int(n);
                self.stack.push(v);
            }
            AddInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 + v2);
                        self.stack.push(v);
                    }
                }
            }
        }
    }

    fn is_finished(&self) -> bool {
        self.pc == self.instrs.len()
    }

    fn run(&mut self) {
        while !self.is_finished() {
            let ins = self.fetch_instr();
            self.eval_instr(ins);
        }
    }

    fn evaluated_value(mut self) -> Value {
        assert!(self.stack.len() == 1);
        self.stack.pop()
    }
}

pub fn eval_expression(expr: &ast::Expr) -> Value {
    let instrs = compiler::compile_expression(expr);
    let mut vm = VM::new(&instrs);
    vm.run();
    vm.evaluated_value()
}
