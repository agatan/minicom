pub mod instr;
pub mod value;
pub mod stack;
pub use self::value::Value;
use self::instr::Instruction;
use self::instr::Instruction::*;

pub struct VM {
    pc: usize,
    stack: stack::Stack,
    vars: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            pc: 0,
            stack: stack::Stack::new(),
            vars: Vec::new(),
        }
    }

    fn fetch_instr(&mut self, instrs: &[Instruction]) -> Instruction {
        let ins = instrs[self.pc];
        debug!("instruction: {:?}", ins);
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
            SetLocal(n) => {
                let v = self.stack.pop();
                debug_assert_eq!(self.vars.len(), n as usize);
                self.vars.push(v);
                self.stack.push(Value::Unit);
            }
            GetLocal(n) => {
                let v = self.vars[n as usize];
                self.stack.push(v);
            }
        }
    }

    fn is_finished(&self, instrs: &[Instruction]) -> bool {
        self.pc == instrs.len()
    }

    pub fn run(&mut self, instrs: &[Instruction]) {
        self.pc = 0;
        while !self.is_finished(instrs) {
            let ins = self.fetch_instr(instrs);
            self.eval_instr(ins);
        }
    }
}
