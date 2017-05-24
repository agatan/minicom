pub mod instr;
pub mod compiler;
pub mod value;
pub mod stack;

pub use self::value::Value;
use self::instr::Instruction;
use self::instr::Instruction::*;

use sem::ir::Expr;

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
            Pop => {
                self.stack.pop();
            }
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
                    _ => panic!("typed expression should be always valid"),
                }
            }
            SubInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 - v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            MulInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 * v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            DivInt => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Int(v1), Value::Int(v2)) => {
                        let v = Value::Int(v1 / v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            PushFloat(n) => {
                let v = Value::Float(n);
                self.stack.push(v);
            }
            AddFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 + v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            SubFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 - v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            MulFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 * v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            DivFloat => {
                let v1 = self.stack.pop();
                let v2 = self.stack.pop();
                match (v1, v2) {
                    (Value::Float(v1), Value::Float(v2)) => {
                        let v = Value::Float(v1 / v2);
                        self.stack.push(v);
                    }
                    _ => panic!("typed expression should be always valid"),
                }
            }
            Print => {
                let v = self.stack.pop();
                println!("{:?}", v);
                self.stack.push(v);
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
}

pub fn eval_expression(exprs: &[Expr]) {
    let instrs = compiler::compile(exprs);
    let mut vm = VM::new(&instrs);
    vm.run();
}
